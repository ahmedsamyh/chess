#include <clock/clock.h>
#include <time.h>

// TODO: Prevent ai from making moves that will leave the king in check
// TODO: Implement Castling
// TODO: Implement Checkmate for user // ai done
// TODO: Implement Transformation of Pawn to any piece except Pawn and King when they are at the base of the opponent
// NOTE: Check is when king is in eatable but can king can move to safety or another piece can shield the king.
// NOTE: Checkmate is when king is in eatable and not the above

typedef struct Piece Piece;
typedef enum Piece_type Piece_type;

static int tile_size;
static const int cols = 8;
static const int rows = 8;
static Rect board_rect = {0};

#define CHAR_SIZE 32

#define WHITE false
#define BLACK true

#define SELECTED_PIECE_COLOR COLOR_NAVY
#define MOVABLE_TILE_COLOR COLOR_GOLD
#define EATABLE_PIECE_COLOR COLOR_MAGENTA
#define CHECKED_PIECE_COLOR COLOR_RED
#define CHECKING_PIECE_COLOR COLOR_CYAN
#define COORDINATE_COLOR COLOR_GRAY
#define BLACK_KING_COLOR COLOR_RED
#define WHITE_KING_COLOR COLOR_GREEN

static Piece* pieces = NULL; // dynamic-array
static Piece** pseudo_removed_pieces = NULL; // dynamic-array
static Piece* selected_piece = NULL;

static Piece* white_checking_piece = NULL;
static Piece* black_checking_piece = NULL;
static Piece* black_king = NULL;
static Piece* white_king = NULL;

static bool white_turn = true;
static int piece_move_speed = 12;

static bool draw_hover_piece = false;
static bool black_check = false;
static bool black_checkmate = false;
static bool white_check = false;
static bool white_checkmate = false;

static bool dev_mode = false;
static bool user_control_all_pieces = false;
static bool ai_control_pieces = true;

static void change_turn(void) {
  if (white_turn) {
    if (ai_control_pieces)
      white_turn = false;
  } else {
    white_turn = true;
  }
}

typedef struct {
  Piece* eating_piece;
  Piece* eated_piece;
} Piece_removal_entry;

static Piece_removal_entry* piece_removal_entries = NULL; // dynamic-array

// TODO: Move this to clock_vector
static bool v2i_eq(Vector2i a, Vector2i b) {
  return (a.x == b.x) && (a.y == b.y);
}

inline Vector2f v2i_to_v2f(Vector2i v2i) {
  return (Vector2f) {(float)v2i.x, (float)v2i.y};
}

Vector2i fix_to_tile_space(Vector2f pos) {
  Vector2i res = {0};
  res.x = ((int)pos.x / tile_size)*tile_size;
  res.y = ((int)pos.y / tile_size)*tile_size;
  return res;
}

inline bool is_pos_in_bounds_in_screen_space(Vector2i pos) {
  return (pos.x >= 0 && pos.x <= (cols-1)*tile_size &&
	  pos.y >= 0 && pos.y <= (rows-1)*tile_size);
}

static char x_coord_strs[] = {
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'
};

static char y_coord_strs[] = {
  '1', '2', '3', '4', '5', '6', '7', '8'
};

// NOTE: Caller must free result
static char* get_coord_str(Vector2i pos) {
  ASSERT(is_pos_in_bounds_in_screen_space(pos));
  pos.x /= tile_size;
  pos.y /= tile_size;

  pos.y = rows - 1 - pos.y;

  ASSERT(0 <= pos.x && pos.x <= cols);
  ASSERT(0 <= pos.y && pos.y <= rows);

  char* str = malloc(sizeof(char)*3);
  str[0] = x_coord_strs[pos.x];
  str[1] = y_coord_strs[pos.y];
  str[2] = '\0';

  return str;
}

enum Piece_type {
  PIECE_TYPE_PAWN,
  PIECE_TYPE_BISHOP,
  PIECE_TYPE_ROOK,
  PIECE_TYPE_KNIGHT,
  PIECE_TYPE_KING,
  PIECE_TYPE_QUEEN,
  PIECE_TYPE_COUNT,
};

inline cstr get_piece_type_str(const Piece_type type) {
  switch (type) {
  case PIECE_TYPE_PAWN: {
    return "Pawn";
  } break;
  case PIECE_TYPE_BISHOP: {
    return "Bishop";
  } break;
  case PIECE_TYPE_ROOK: {
    return "Rook";
  } break;
  case PIECE_TYPE_KNIGHT: {
    return "Knight";
  } break;
  case PIECE_TYPE_KING: {
    return "King";
  } break;
  case PIECE_TYPE_QUEEN: {
    return "Queen";
  } break;
  default: ASSERT(0 && "Unreachable!");
  }
  return "THIS SHOULD NOT BE VISIBLE!";
}

typedef enum {
  UNDO_CMD_TYPE_MOVE,
  UNDO_CMD_TYPE_SPAWN,
  UNDO_CMD_TYPE_REMOVE,
  UNDO_CMD_TYPE_COUNT,
} Undo_cmd_type;

typedef struct {
  Undo_cmd_type type;
  Vector2i piece_pos;
  Piece_type piece_type;
  bool piece_black;
  Piece* moving_piece;
  bool was_first_move;
} Undo_cmd;

#define UNDO_CMD_STACK_CAP (1024*2)
typedef struct {
  Undo_cmd buff[UNDO_CMD_STACK_CAP];
  size_t count;
} Undo_cmd_stack;

void Undo_cmd_stack_push(Undo_cmd_stack* stack, Undo_cmd value) {
  ASSERT((stack->count + 1) <= UNDO_CMD_STACK_CAP);

  stack->buff[stack->count++] = value;
}

bool Undo_cmd_stack_pop(Undo_cmd_stack* stack, Undo_cmd* popped) {
  if (stack->count == 0) {
    return false;
  }

  if (popped) {
    *popped = stack->buff[--stack->count];
  } else {
    --stack->count;
  }

  return true;
}

bool Undo_cmd_stack_top(Undo_cmd_stack* stack, Undo_cmd* top) {
  if (stack->count == 0) {
    return false;
  }

  *top = stack->buff[stack->count - 1];

  return true;
}

static Undo_cmd_stack undo_cmd_stack = {0};

/* typedef struct { */
/*   int key; */
/*   Piece* value; */
/* } Undo_cmd_move_correction_KV; */

/* static Undo_cmd_move_correction_KV* undo_cmd_move_corrections = NULL; // hashmap */

static void correct_undo_move(Piece* spawned_piece) {
  if (undo_cmd_stack.count == 0) return;
  for (int i = (int)(undo_cmd_stack.count-1); i >= 0; --i) {
    Undo_cmd* cmd = &undo_cmd_stack.buff[i];
    if (cmd->type == UNDO_CMD_TYPE_MOVE) {
      cmd->moving_piece = spawned_piece;
      return;
    }
  }
  ASSERT(0 && "Unreachable!");
}

struct Piece {
  Piece_type type;
  Vector2i pos;
  Vector2i to;
  int height;
  Sprite spr;
  Context* ctx;
  bool black;
  bool moved_once;
  bool moving;
};

inline void select_piece(Piece* piece) {
  if (selected_piece) selected_piece->height = 0;
  selected_piece = piece;
  /* if (selected_piece) selected_piece->height = (int)(tile_size*0.25f); */
}

inline Piece** get_black_piece_ptrs(void) {
  Piece** black_piece_ptrs = NULL;

  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (pieces[i].black) {
      arrput(black_piece_ptrs, &pieces[i]);
    }
  }

  return black_piece_ptrs;
}

inline Piece** get_white_piece_ptrs(void) {
  Piece** white_piece_ptrs = NULL;

  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (!pieces[i].black) {
      arrput(white_piece_ptrs, &pieces[i]);
    }
  }

  return white_piece_ptrs;
}

static void reverse_offsets_y(Vector2i* offsets) {
  for (int i = 0; i < arrlenu(offsets); ++i) {
    offsets[i].y *= -1;
  }
}

Piece* get_piece_at_pos(Vector2i pos) {
  for (int i = 0; i < arrlenu(pieces); ++i) {
    Vector2i p = pieces[i].moving ? pieces[i].to : pieces[i].pos;
    if (v2i_eq(pos, p)) {
      return &pieces[i];
    }
  }
  return NULL;
}

// actually doesn't eat the piece; just adds a reference to it
#define EAT_PIECE() \
  Piece* existing_piece = get_piece_at_pos(v2i_add(piece->pos, v2i_muls(offset, tile_size))); \
  if (existing_piece &&							\
      existing_piece->black != piece->black) {				\
    arrput(res.eatable_piece_ptrs, existing_piece);			\
  }

#define BREAK_IF_PIECE_EXISTS()						\
  Vector2i pos = v2i_add(piece->pos, v2i_muls(offset, tile_size));	\
  if (get_piece_at_pos(pos)) break

#define BREAK_AND_EAT_PIECE() \
  EAT_PIECE();		      \
  BREAK_IF_PIECE_EXISTS()

#define BISHOP_MOVEMENTS(dx, dy)					\
  for (int i = 1; i <= rows-1; ++i) {					\
    Vector2i offset = {dx*i, dy*i};					\
    BREAK_AND_EAT_PIECE();						\
    arrput(offsets, offset);						\
  }

#define ROOK_MOVEMENTS(dx, dy)			\
  for (int i = 1; i <= rows-1; ++i) {		\
    Vector2i offset = {dx*i, dy*i};		\
    BREAK_AND_EAT_PIECE();			\
    arrput(offsets, offset);			\
  }

typedef struct {
  Vector2i* movements; // dynamic-array
  Piece** eatable_piece_ptrs; // dynamic-array
} Movement_result;

inline void free_movement_results(Movement_result mr) {
  if (mr.movements) arrfree(mr.movements);
  if (mr.eatable_piece_ptrs) arrfree(mr.eatable_piece_ptrs);
}

// caller must be responsible for freeing the result
Movement_result get_piece_moves(Piece* piece) {
  Movement_result res = {0};
  Vector2i* offsets = NULL; // dynamic-array
  switch (piece->type) {
  case PIECE_TYPE_PAWN: {
    for (int i = 1; i <= (piece->moved_once ? 1 : 2); ++i) {
      Vector2i offset = {0, piece->black ? i : -i};
      BREAK_IF_PIECE_EXISTS();
      arrput(offsets, offset);
    }
  } break;
  case PIECE_TYPE_BISHOP: {
    BISHOP_MOVEMENTS(-1, -1);
    BISHOP_MOVEMENTS( 1, -1);
    BISHOP_MOVEMENTS( 1,  1);
    BISHOP_MOVEMENTS(-1,  1);
  } break;
  case PIECE_TYPE_ROOK: {
    ROOK_MOVEMENTS(-1,  0);
    ROOK_MOVEMENTS( 1,  0);
    ROOK_MOVEMENTS( 0, -1);
    ROOK_MOVEMENTS( 0,  1);
  } break;
  case PIECE_TYPE_KNIGHT: {
    for (int i = -1; i <= 1; ++i) {
      for (int j = -1; j <= 1; ++j) {
	if (i != 0 && j != 0) {
	  Vector2i offset = {i, j*2};
	  {
	    arrput(offsets, offset);
	  }
	  {
	    offset = (Vector2i) {i*2, j*1};
	    arrput(offsets, offset);
	  }
	}
      }
    }
  } break;
  case PIECE_TYPE_KING: {
    for (int x = -1; x <= 1; ++x) {
      for (int y = -1; y <= 1; ++y) {
	if (x != 0 || y != 0) {
	  Vector2i offset = {x, y};
	  arrput(offsets, offset);
	}
      }
    }
  } break;
  case PIECE_TYPE_QUEEN: {
    BISHOP_MOVEMENTS(-1, -1);
    BISHOP_MOVEMENTS( 1, -1);
    BISHOP_MOVEMENTS( 1,  1);
    BISHOP_MOVEMENTS(-1,  1);

    ROOK_MOVEMENTS(-1,  0);
    ROOK_MOVEMENTS( 1,  0);
    ROOK_MOVEMENTS( 0, -1);
    ROOK_MOVEMENTS( 0,  1);
  } break;
  default: ASSERT(0 && "Unreachable");
  }

  size_t offsets_count = arrlenu(offsets);
  for (int i = 0; i < offsets_count; ++i) {
    Vector2i offset = offsets[i];
    Vector2i pos = v2i_add(piece->pos, v2i_muls(offset, tile_size));
    if (is_pos_in_bounds_in_screen_space(pos)) {
      Piece* existing_piece = get_piece_at_pos(pos);
      if (existing_piece) {
	if (existing_piece->black != piece->black)
	  arrput(res.eatable_piece_ptrs, existing_piece);
      } else {
	arrput(res.movements, pos);
      }
    }
  }

  // pawn can eat diagonally
  if (piece->type == PIECE_TYPE_PAWN) {
    for (int i = 0; i < 2; ++i) {
      Vector2i offset = {i == 0 ? -1 : 1, piece->black ? 1 : -1};
      Vector2i pos = v2i_add(piece->pos, v2i_muls(offset, tile_size));
      Piece* existing_piece = get_piece_at_pos(pos);
      if (existing_piece &&
	  existing_piece->black != piece->black) {
	// eatable enemy
	arrput(res.eatable_piece_ptrs, existing_piece);
      }
    }
  }

  arrfree(offsets);

  return res;
}

bool change_piece_type(Piece* piece, const Piece_type type) {
  ASSERT(piece->ctx);
  piece->type = type;

  Texture* tex = Resman_load_texture_from_file(piece->ctx->resman, "resources/gfx/piece_sheet.png");

  if (!tex) return false;

  if (!Sprite_init(&piece->spr, tex, PIECE_TYPE_COUNT*2, 1)) return false;

  piece->spr.scale.x = (float)(piece->ctx->win->width/480);
  piece->spr.scale.y = (float)(piece->ctx->win->height/480);

#define X(idx) ((idx)*2 + (piece->black ? 1 : 0))
  switch (type) {
  case PIECE_TYPE_PAWN: {
    Sprite_set_hframe(&piece->spr, X(0));
  } break;
  case PIECE_TYPE_BISHOP: {
    Sprite_set_hframe(&piece->spr, X(1));
  } break;
  case PIECE_TYPE_ROOK: {
    Sprite_set_hframe(&piece->spr, X(2));
  } break;
  case PIECE_TYPE_KNIGHT: {
    Sprite_set_hframe(&piece->spr, X(3));
  } break;
  case PIECE_TYPE_KING: {
    Sprite_set_hframe(&piece->spr, X(4));
  } break;
  case PIECE_TYPE_QUEEN: {
    Sprite_set_hframe(&piece->spr, X(5));
  } break;
  default: ASSERT(0 && "Unreachable!");
  }

  Sprite_center_origin(&piece->spr);

  return true;
}

bool init_piece(Piece* piece, Context* ctx, const Piece_type type, bool black) {
  piece->ctx = ctx;
  piece->pos = (Vector2i) {0, 0};
  piece->black = black;
  if (!change_piece_type(piece, type)) {
    return false;
  }
  return true;
}

void deinit_piece(Piece* piece) {
  (void)piece;
  /* Sprite_deinit(&piece->spr); */
}

void draw_piece(Piece* piece) {
  ASSERT(piece->ctx);

  piece->spr.pos = (Vector2f) {(float)piece->pos.x + tile_size/2.f, (float)piece->pos.y + tile_size/2.f};
  piece->spr.pos.y -= piece->height;

  draw_sprite(piece->ctx, &piece->spr);
}

Vector2i tile_to_screen_space(Vector2i tile_space) {
  ASSERT(0 <= tile_space.x && tile_space.x < cols);
  ASSERT(0 <= tile_space.y && tile_space.y < rows);
  return v2i_muls(tile_space, tile_size);
}

bool animate_piece_moving(int delta) {
  bool moving = false;
  for (int i = 0; i < arrlenu(pieces); ++i) {
    Piece* piece = &pieces[i];
    if (piece->moving) {
      Vector2i dir = {0, 0};
      int diff = piece->to.x - piece->pos.x;
      if (abs(diff) < abs(piece_move_speed)) {
	piece->pos.x = piece->to.x;
      } else {
	if (diff < 0) {
	  dir.x = -piece_move_speed;
	} else if (diff > 0) {
	  dir.x = piece_move_speed;
	}
      }

      diff = piece->to.y - piece->pos.y;
      if (abs(diff) < abs(piece_move_speed)) {
	piece->pos.y = piece->to.y;
      } else {
	if (diff < 0) {
	  dir.y = -piece_move_speed;
	} else if (diff > 0) {
	  dir.y = piece_move_speed;
	}
      }

      dir = v2i_muls(dir, delta);

      piece->pos = v2i_add(piece->pos, dir);

      if (v2i_eq(piece->pos, piece->to)) {
	piece->moving = false;
      }
      moving = true;
    }
  }
  return moving;
}

inline void offset_piece_when_removing_piece(Piece** piece, Piece* removing_piece) {
  if ((*piece) > removing_piece) {
    (*piece)--;
  }
}

void pseudo_remove_piece(Piece* removing_piece) {
  arrput(pseudo_removed_pieces, removing_piece);
}

void pseudo_unremove_piece(Piece* unremoving_piece) {
  for (int i = 0; i < arrlenu(pseudo_removed_pieces); ++i) {
    if (unremoving_piece == pseudo_removed_pieces[i]) {
      arrdel(pseudo_removed_pieces, i);
      break;
    }
  }
}

bool is_piece_pseudo_removed(Piece* piece) {
  for (int i = 0; i < arrlenu(pseudo_removed_pieces); ++i) {
    if (piece == pseudo_removed_pieces[i]) {
      return true;
    }
  }
  return false;
}

// TODO (speed): better way to remove piece
void remove_piece(Piece* removing_piece) {
  // NOTE: Doesn't leak
  Undo_cmd_stack_push(&undo_cmd_stack, (Undo_cmd) {
      .type = UNDO_CMD_TYPE_SPAWN,
      .piece_pos = removing_piece->pos,
      .piece_type = removing_piece->type,
      .piece_black = removing_piece->black,
    });

  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (&pieces[i] == removing_piece) {
      deinit_piece(&pieces[i]);
      arrdel(pieces, i);
    }
  }

  offset_piece_when_removing_piece(&black_king, removing_piece);
  offset_piece_when_removing_piece(&white_king, removing_piece);
  if (white_checking_piece) offset_piece_when_removing_piece(&white_checking_piece, removing_piece);
  if (black_checking_piece) offset_piece_when_removing_piece(&black_checking_piece, removing_piece);
}


static bool get_move_idx(Piece* piece, Vector2i to, int* movement_idx, int* eating_piece_idx) {
  ASSERT(piece);

  bool found = false;

  Movement_result mr = get_piece_moves(piece);
  size_t movements_count = arrlenu(mr.movements);
  if (movements_count > 0) {
    for (int i = 0; i < movements_count; ++i) {
      Vector2i p = mr.movements[i];
      if (v2i_eq(p, to)) {
	*movement_idx = i;
	found = true;
	break;
      }
    }
  }


  if (!found) {
    size_t eatable_pieces_count = arrlenu(mr.eatable_piece_ptrs);
    if (eatable_pieces_count > 0) {
      for (int i = 0; i < eatable_pieces_count; ++i) {
	Piece* p = mr.eatable_piece_ptrs[i];
	if (v2i_eq(p->pos, to)) {
	  *eating_piece_idx = i;
	  found = true;
	  break;
	}
      }
    }
  }

  free_movement_results(mr);
  return found;
}

// NOTE: mr should not be freed in this function!!!
static bool move_piece_to(Piece** piece_ptr, Vector2i to, Movement_result mr, bool record_undo) {
  Piece* piece = *piece_ptr;
  if (v2i_eq(piece->pos, to)) return false;
  if (!piece) return false;

  bool moved = false;

  Piece* eated_piece = NULL;
  bool remove_eated_piece = false;
  // eat
  for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) {
    eated_piece = mr.eatable_piece_ptrs[i];
    if (v2i_eq(eated_piece->pos, to)) {
      piece->to = to;
      piece->moving = true;
      moved = true;
      remove_eated_piece = true;
      if (record_undo) {
	// NOTE: Doesn't leak
	Undo_cmd_stack_push(&undo_cmd_stack, (Undo_cmd) {
	    .type = UNDO_CMD_TYPE_MOVE,
	    .piece_pos = piece->pos,
	    .moving_piece = piece,
	    .was_first_move = !piece->moved_once,
	  });
      }
      break;
    }
  }

  if (remove_eated_piece &&
      eated_piece) {
    ASSERT(eated_piece != piece);

    Piece_removal_entry entry = {
      .eating_piece = piece,
      .eated_piece = eated_piece
    };

    ASSERT(entry.eating_piece->black != entry.eated_piece->black);
    arrput(piece_removal_entries, entry);
  }

  // move
  for (int i = 0; i < arrlenu(mr.movements); ++i) {
    Vector2i pos = mr.movements[i];
    if (v2i_eq(pos, piece->pos)) continue;
    if (v2i_eq(pos, to)) {
      piece->to = to;
      piece->moving = true;
      moved = true;
      if (record_undo) {
	Undo_cmd_stack_push(&undo_cmd_stack, (Undo_cmd) {
	    .type = UNDO_CMD_TYPE_MOVE,
	    .piece_pos = piece->pos,
	    .moving_piece = piece,
	    .was_first_move = !piece->moved_once,
	  });
      }
      break;
    }
  }

  if (!piece->moved_once && moved) {
    piece->moved_once = true;
  }

  return moved;
}

// NOTE: forward declaration...
bool is_piece_in_danger(Piece* piece);
static void filter_moves_that_will_not_protect_king(Piece* piece, Movement_result* mr) {
  size_t movements_count = arrlenu(mr->movements);
  for (int i = (int)(movements_count-1); i >= 0; --i) {
    Vector2i move = mr->movements[i];
    Vector2i prev_pos = piece->pos;
    ASSERT(move_piece_to(&piece, move, *mr, false));

    if (is_piece_in_danger(white_king)) {
      arrdel(mr->movements, i);
      movements_count = arrlenu(mr->movements);
    }

    piece->moving = false;
    piece->to = prev_pos;
  }
}

bool spawn_piece(Context* ctx, const Piece_type type, Vector2i pos, bool black) {
  Piece p = {0};
  if (!init_piece(&p, ctx, type, black)) {
    return false;
  }

  p.pos = pos;

  arrput(pieces, p);

  return true;
}

bool add_piece(Context* ctx, const Piece_type type, int y_offset, int* xs, size_t xs_size) {
  Piece p = {0};
  // white
  for (int i = 0; i < xs_size; ++i) {
    if (!init_piece(&p, ctx, type, WHITE)) return false;
    p.pos = tile_to_screen_space((Vector2i) {xs[i], 8-1-y_offset});

    arrput(pieces, p);
  }
  // black
  for (int i = 0; i < xs_size; ++i) {
    if (!init_piece(&p, ctx, type, BLACK)) return false;
    p.pos = tile_to_screen_space((Vector2i) {(cols-1) - xs[i], y_offset});

    arrput(pieces, p);
  }

  return true;
}

void reset_variables(void) {
  draw_hover_piece = false;
  select_piece(NULL);
  black_checkmate = false;
  black_check = false;
  white_checkmate = false;
  white_check = false;
}

bool is_piece_in_danger(Piece* piece) {
  if (!piece->black) {
    Piece** black_piece_ptrs = get_black_piece_ptrs();

    black_checking_piece = NULL;
    for (int i = 0; i < arrlen(black_piece_ptrs); ++i) {
      if (is_piece_pseudo_removed(black_piece_ptrs[i])) continue;
      Movement_result mr = get_piece_moves(black_piece_ptrs[i]);

      for (int j = 0; j < arrlenu(mr.eatable_piece_ptrs); ++j) {
	if (mr.eatable_piece_ptrs[j] == piece) {
	  ASSERT(!piece->black);
	  black_checking_piece = black_piece_ptrs[i];
	}
      }

      free_movement_results(mr);
      if (black_checking_piece != NULL) break;
    }

    arrfree(black_piece_ptrs);

    return black_checking_piece != NULL;
  } else {
    Piece** white_piece_ptrs = get_white_piece_ptrs();

    white_checking_piece = NULL;
    for (int i = 0; i < arrlen(white_piece_ptrs); ++i) {
      if (is_piece_pseudo_removed(white_piece_ptrs[i])) continue;
      Movement_result mr = get_piece_moves(white_piece_ptrs[i]);

      for (int j = 0; j < arrlenu(mr.eatable_piece_ptrs); ++j) {
	if (mr.eatable_piece_ptrs[j] == piece) {
	  ASSERT(piece->black);
	  white_checking_piece = white_piece_ptrs[i];
	}
      }

      free_movement_results(mr);
      if (white_checking_piece != NULL) break;
    }

    arrfree(white_piece_ptrs);

    return white_checking_piece != NULL;
  }



  /* for (int i = 0; i < arrlenu(pieces); ++i) { */
  /*   if (pieces[i].black == piece->black) continue; */

  /*   Movement_result mr = get_piece_moves(&pieces[i]); */

  /*   for (int j = 0; j < arrlenu(mr.eatable_piece_ptrs); ++j) { */
  /*     if (mr.eatable_piece_ptrs[j] == piece) { */
  /* 	if (piece->black) { */
  /* 	  white_checking_piece = &pieces[i]; */
  /* 	  black_checking_piece = NULL; */
  /* 	} else { */
  /* 	  black_checking_piece = &pieces[i]; */
  /* 	  white_checking_piece = NULL; */
  /* 	} */
  /* 	return true; */
  /*     } */
  /*   } */

  /*   free_movement_results(mr); */
  /* } */
  /* return false; */
}

/* bool move_is_check(Piece* piece, Piece** checked_piece) { */
/*   Movement_result mr = get_piece_moves(piece); */

/*   for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) { */
/*     Piece* eating_piece = mr.eatable_piece_ptrs[i]; */
/*     if (piece->black != eating_piece->black && */
/* 	eating_piece->type == PIECE_TYPE_KING) { */
/*       *checked_piece = eating_piece; */
/*       return true; */
/*     } */
/*   } */

/*   free_movement_results(mr); */

/*   return false; */
/* } */

bool init_pieces(Context* ctx) {
  reset_variables();
  arrsetlen(pieces, 0);

  // pawns
  {
    int xs[] = {0, 1, 2, 3, 4, 5, 6, 7};
    if (!add_piece(ctx, PIECE_TYPE_PAWN, 1, xs, ARRAY_LEN(xs))) return false;
  }

  // bishops
  {
    int xs[] = {2, 7-2};
    if (!add_piece(ctx, PIECE_TYPE_BISHOP, 0, xs, ARRAY_LEN(xs))) return false;
  }

  // rooks
  {
    int xs[] = {0, 7-0};
    if (!add_piece(ctx, PIECE_TYPE_ROOK, 0, xs, ARRAY_LEN(xs))) return 1;
  }

  // knights
  {
    int xs[] = {1, 7-1};
    if (!add_piece(ctx, PIECE_TYPE_KNIGHT, 0, xs, ARRAY_LEN(xs))) return 1;
  }

  // kings
  {
    int xs[] = {4};
    if (!add_piece(ctx, PIECE_TYPE_KING, 0, xs, ARRAY_LEN(xs))) return false;
    black_king = &pieces[arrlen(pieces)-1];
    white_king = &pieces[arrlen(pieces)-2];
  }

  // queens
  {
    int xs[] = {3};
    if (!add_piece(ctx, PIECE_TYPE_QUEEN, 0, xs, ARRAY_LEN(xs))) return false;
  }

  return true;
}

inline int get_random_idx(int arr_count) {
  ASSERT(arr_count > 0);
  return arr_count == 1 ? 0 : (rand() % (arr_count));
}

void ai_choose_piece(Piece** selecting_piece) {
  Piece** black_piece_ptrs = get_black_piece_ptrs();

  ASSERT(arrlenu(black_piece_ptrs) > 0);

  int random_idx = get_random_idx((int)arrlenu(black_piece_ptrs));

  ASSERT(0 <= random_idx && random_idx <= (arrlenu(black_piece_ptrs) - 1));

  *selecting_piece = black_piece_ptrs[random_idx];

  arrfree(black_piece_ptrs);
}

void ai_choose_move(Piece* selecting_piece, Movement_result mr) {
  int movements_count = (int)arrlenu(mr.movements);

  int eatable_piece_ptrs_count = (int)arrlenu(mr.eatable_piece_ptrs);

  if (movements_count > 0 && eatable_piece_ptrs_count > 0) {
    if ((rand() % 100) <= 50) {
      int random_idx = get_random_idx(movements_count);
      Vector2i random_move = mr.movements[random_idx];
      move_piece_to(&selecting_piece, random_move, mr, true);
    } else {
      int random_idx = get_random_idx(eatable_piece_ptrs_count);
      Vector2i random_move = mr.eatable_piece_ptrs[random_idx]->pos;
      move_piece_to(&selecting_piece, random_move, mr, true);
    }
    return;
  }

  if (movements_count > 0) {
    int random_idx = get_random_idx(movements_count);
    Vector2i random_move = mr.movements[random_idx];
    move_piece_to(&selecting_piece, random_move, mr, true);
    return;
  }

  if (eatable_piece_ptrs_count > 0) {
    int random_idx = get_random_idx(eatable_piece_ptrs_count);
    Vector2i random_move = mr.eatable_piece_ptrs[random_idx]->pos;
    move_piece_to(&selecting_piece, random_move, mr, true);
    return;
  }

  ASSERT(0 && "Unreachable!");
}

/* // TODO: Optimize */

// ## Ways to get out of Check
// In order of precedence
// -> any piece can eat the checking piece
// -> other piece can protect king
// -> king can move out of danger

inline bool tried_idx(int* tried_indices, int idx) {
  for (int i = 0; i < arrlenu(tried_indices); ++i) {
    if (tried_indices[i] == idx) {
      return true;
    }
  }
  return false;
}

static bool ai_checked_king_move_out_of_danger(void) {
  ASSERT(black_king);
  Movement_result mr = get_piece_moves(black_king);

  if (arrlenu(mr.movements) == 0) {
    free_movement_results(mr);
    return false;
  } else {
    int* tried_indices = NULL; // dynamic-array
  checked_king_move_out_of_danger_retry:
    if (arrlenu(tried_indices) == arrlenu(mr.movements)) {
      arrfree(tried_indices);
      free_movement_results(mr);
      return false;
    }

    int movements_count = (int)arrlen(mr.movements);
    int random_idx = get_random_idx(movements_count);
    while (tried_idx(tried_indices, random_idx)) {
      random_idx = get_random_idx(movements_count);
    }

    Vector2i prev_pos = black_king->moving ? black_king->to : black_king->pos;
    move_piece_to(&black_king, mr.movements[random_idx], mr, true);

    if (is_piece_in_danger(black_king)) {
      Undo_cmd_stack_pop(&undo_cmd_stack, NULL);
      black_king->to = prev_pos;
      arrput(tried_indices, random_idx);
      goto checked_king_move_out_of_danger_retry;
    } else {
      // TODO: Maybe push the undo here?...
    }

    arrfree(tried_indices);
  }

  free_movement_results(mr);

  return true;
}

inline bool does_piece_movements_collide(Piece* a, Piece* b, Vector2i* colliding_pos) {
    Movement_result a_mr = get_piece_moves(a);
    Movement_result b_mr = get_piece_moves(b);

    bool res = false;

    size_t a_movements_count = arrlenu(a_mr.movements);
    size_t b_movements_count = arrlenu(b_mr.movements);
    for (int i = 0; i < a_movements_count; ++i) {
      for (int j = 0; j < b_movements_count; ++j) {
	if (v2i_eq(a_mr.movements[i], b_mr.movements[j])) {
	  *colliding_pos = a_mr.movements[i];
	  res = true;
	  goto does_piece_movements_collide_done;
	}
      }
    }
 does_piece_movements_collide_done:
    free_movement_results(a_mr);
    free_movement_results(b_mr);

    return res;
}

static bool ai_protect_checked_king(void) {
  ASSERT(white_checking_piece);

  Piece** piece_ptrs = get_black_piece_ptrs();
  size_t piece_ptrs_count = arrlenu(piece_ptrs);

  bool protected = false;

  for (int i = 0; i < piece_ptrs_count; ++i) {
    Piece* p = piece_ptrs[i];
    Vector2i colliding_pos = {0};
    if (does_piece_movements_collide(p, white_checking_piece, &colliding_pos)) {
      Vector2i prev_pos = p->pos;
      p->to = colliding_pos;
      char* to_str = get_coord_str(p->to);
      free(to_str);
      p->moving = true;
      if (is_piece_in_danger(black_king)) {
	p->to = prev_pos;
	p->moving = false;
      } else {
	protected = true;
	// NOTE: Doesn't leak
	Undo_cmd_stack_push(&undo_cmd_stack, (Undo_cmd) {
	  .type = UNDO_CMD_TYPE_MOVE,
	  .piece_pos = p->pos,
	  .moving_piece = p,
	  .was_first_move = !p->moved_once
	});
	break;
      }
    }
  }

  arrfree(piece_ptrs);
  return protected;
}

static bool ai_eat_checking_piece(void) {
  ASSERT(white_checking_piece);

  Piece** piece_ptrs = get_black_piece_ptrs();
  size_t piece_ptrs_count = arrlenu(piece_ptrs);

  bool ate_checking_piece = false;
  Piece* eating_piece = NULL;

  for (int i = 0; i < piece_ptrs_count; ++i) {
    Piece* p = piece_ptrs[i];
    Movement_result mr = get_piece_moves(p);

    size_t eatable_piece_ptrs_count = arrlenu(mr.eatable_piece_ptrs);
    for (int j = 0; j < eatable_piece_ptrs_count; ++j) {
      if (mr.eatable_piece_ptrs[j] == white_checking_piece) {
	eating_piece = p;
	p->to = white_checking_piece->pos;
	p->moving = true;
	// NOTE: Doesn't leak
	Undo_cmd_stack_push(&undo_cmd_stack, (Undo_cmd) {
	    .type = UNDO_CMD_TYPE_MOVE,
	    .piece_pos = p->pos,
	    .moving_piece = p,
	    .was_first_move = !p->moved_once,
	  });
        ate_checking_piece = true;
	free_movement_results(mr);
	goto done;
      }
    }

    free_movement_results(mr);
  }
 done:
  arrfree(piece_ptrs);

  if (ate_checking_piece) {
    ASSERT(eating_piece);
    Piece_removal_entry entry =  {
      .eating_piece = eating_piece,
      .eated_piece = white_checking_piece
    };
    arrput(piece_removal_entries, entry);
    white_checking_piece = NULL;
  }

  return ate_checking_piece;
}

void ai_control_piece(Context* ctx) {
  if (black_check) {
    if (ai_eat_checking_piece()) {
      log_info("Other piece ate checking piece!");
      change_turn();
      return;
    }

    if (ai_protect_checked_king()) {
      log_info("Other piece protected checked king!");
      change_turn();
      return;
    }

    if (ai_checked_king_move_out_of_danger()) {
      log_info("King moved out of danger!");
      change_turn();
      return;
    }

    black_checkmate = true;

    return;
  }

  (void)ctx;
  Movement_result mr = {0};

 ai_rechoose_move:
  while (arrlenu(mr.movements) == 0 && arrlenu(mr.eatable_piece_ptrs) == 0) {
    ai_choose_piece(&selected_piece);

    free_movement_results(mr);

    mr = get_piece_moves(selected_piece);
  }
  if (!selected_piece->black) {
    goto ai_rechoose_move;
  }

  Vector2i moving_from = selected_piece->pos;
  ASSERT(selected_piece->black);

  ai_choose_move(selected_piece, mr);


  char* moving_from_coord = get_coord_str(moving_from);
  char* moved_to_coord = get_coord_str(selected_piece->to);

  log_info("Moved %s %s Piece from %s to %s", (selected_piece->black ? "Black" : "White"), get_piece_type_str(selected_piece->type),
	   moving_from_coord,
	   moved_to_coord);

  free(moving_from_coord);
  free(moved_to_coord);

  select_piece(NULL);

  if (!selected_piece) {
    change_turn();
  }
}

// NOTE: mr should not be freed in this function!!!
bool will_move_protect_king(Piece* piece, int movement_idx, int eatable_piece_idx, Movement_result mr) {
  ASSERT(!piece->black);
  /* ASSERT(white_check); */
  bool will_protect = false;
  if (movement_idx >= 0) {
    size_t movements_count = arrlenu(mr.movements);
#ifdef LOG_DEBUG_INFO
    log_info("[%s] Piece: %p, movements_count: %u", __func__, piece, movements_count);
#endif
    ASSERT(eatable_piece_idx < 0);
    if (movement_idx >= movements_count) {
      DebugBreak();
    }

    Vector2i prev_pos = piece->moving ? piece->to : piece->pos;
    move_piece_to(&piece, mr.movements[movement_idx], mr, false);

    will_protect = !is_piece_in_danger(white_king);
    piece->to = prev_pos;
  } else if (eatable_piece_idx >= 0) {
    size_t eatable_pieces_count = arrlenu(mr.eatable_piece_ptrs);
    ASSERT(movement_idx < 0);
    ASSERT(eatable_piece_idx < eatable_pieces_count);

    pseudo_remove_piece(mr.eatable_piece_ptrs[eatable_piece_idx]);
    Vector2i prev_pos = piece->moving ? piece->to : piece->pos;
    move_piece_to(&piece, mr.movements[movement_idx], mr, false);

    will_protect = !is_piece_in_danger(white_king);
    piece->to = prev_pos;

    pseudo_unremove_piece(mr.eatable_piece_ptrs[eatable_piece_idx]);
  }
  return will_protect;
}

bool can_piece_protect_king(Piece* piece) {
  bool can_protect = false;
  Movement_result mr = get_piece_moves(piece);
  size_t movements_count = arrlenu(mr.movements);
#ifdef LOG_DEBUG_INFO
  log_info("[%s] Piece: %p, movements_count: %u", __func__, piece, movements_count);
#endif
  for (int i = 0; i < movements_count; ++i) {
    if (will_move_protect_king(piece, i, -1, mr)) {
      can_protect = true;
      break;
    }
  }
  if (!can_protect) {
    size_t eatable_pieces_count = arrlenu(mr.eatable_piece_ptrs);
    for (int i = 0; i < eatable_pieces_count; ++i) {
      if (will_move_protect_king(piece, -1, i, mr)) {
	can_protect = true;
	break;
      }
    }
  }

  free_movement_results(mr);
  return can_protect;
}

void user_control_piece(Context* ctx, bool can_control_black) {
  if (clock_mouse_pressed(ctx, MOUSE_BUTTON_LEFT)) {
    if (selected_piece) {
#ifdef LOG_DEBUG_INFO
      if (!selected_piece->black) {
	int movement_idx = -1, eating_piece_idx = -1;
	if (get_move_idx(selected_piece, fix_to_tile_space(ctx->mpos), &movement_idx, &eating_piece_idx)) {
	  if (movement_idx >= 0) log_info("movement_idx: %d", movement_idx);
	  if (eating_piece_idx >= 0) log_info("eating_piece_idx: %d", eating_piece_idx);

	  if (will_move_protect_king(selected_piece, movement_idx, eating_piece_idx)) {
	    log_info("Move will protect king!");
	  } else {
	    log_info("Move will not protect king!");
	  }
	}
      }
#endif
      Movement_result mr = get_piece_moves(selected_piece);
      if (white_check) {
	filter_moves_that_will_not_protect_king(selected_piece, &mr);
      }
      if (move_piece_to(&selected_piece, fix_to_tile_space(ctx->mpos), mr, true)) {
	select_piece(NULL);
	change_turn();
      }
      free_movement_results(mr);
    } else {
      select_piece(get_piece_at_pos(fix_to_tile_space(ctx->mpos)));
      if (white_check &&
	  selected_piece &&
	  !selected_piece->black &&
	  !can_piece_protect_king(selected_piece)) {
	select_piece(NULL);
      }

      if (!can_control_black) {
	if (selected_piece && selected_piece->black) select_piece(NULL);
      }
    }
    draw_hover_piece = false;
  }

  if (clock_mouse_pressed(ctx, MOUSE_BUTTON_RIGHT)) {
    select_piece(NULL);
    draw_hover_piece = false;
  }
}

static void remove_pieces_marked_for_removal(void) {
  int piece_removal_entries_count = (int)arrlen(piece_removal_entries);
  for (int i = piece_removal_entries_count-1; i >= 0; --i) {
    Piece_removal_entry entry = piece_removal_entries[i];
    ASSERT(entry.eating_piece);
    ASSERT(entry.eated_piece);
    if (v2i_eq(entry.eating_piece->pos, entry.eated_piece->pos)) {
      remove_piece(entry.eated_piece);
      arrdel(piece_removal_entries, i);
    }
  }
}

static void draw_check(Context* ctx, Piece* checking_piece, Piece* checked_king) {
  if (!checking_piece) return;
  if (!checked_king) return;
  Vector2i pos = checked_king->pos;
  draw_box(ctx, (Rect) {
      .pos = (Vector2f) {(float)pos.x, (float)pos.y},
      .size = (Vector2f) {(float)tile_size, (float)tile_size}
    },
    CHECKED_PIECE_COLOR,
    color_alpha(CHECKED_PIECE_COLOR, 0.3f));

  pos = checking_piece->pos;
  draw_box(ctx, (Rect) {
      .pos = (Vector2f) {(float)pos.x, (float)pos.y},
      .size = (Vector2f) {(float)tile_size, (float)tile_size}
    },
    CHECKING_PIECE_COLOR,
    color_alpha(CHECKING_PIECE_COLOR, 0.3f));
}

inline bool is_tile_white(Vector2i pos) {
  Vector2i n = v2i_divs(pos, tile_size);
  if (n.y % 2 != n.x % 2)
    return false;
  return true;
}

static void draw_board(Context* ctx, Font* font, Sprite* selected_tile_spr, Sprite* hovering_piece_sprite) {

  clock_use_camera_view(ctx, true);

  // draw checker pattern
  for (int y = 0; y < rows; ++y) {
    for (int x = 0; x < cols; ++x) {
      Color col = color_from_hex(0XFFF0F6F0);
      if (y % 2 != x % 2) col = color_from_hex(0xFF232322);
      draw_rect(ctx, (Rect) {
	  .pos = (Vector2f) {(float)x*tile_size, (float)y*tile_size},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}},
	col);
    }
  }

  // draw coords
  for (int y = 0; y < rows; ++y) {
    for (int x = 0; x < cols; ++x) {
      Vector2f pos_in_screen_space = {x*tile_size*1.f, y*tile_size*1.f};

      cstr coord_str = "";
      temp_sprintf(coord_str, "%c%c", x_coord_strs[x], y_coord_strs[rows-1-y]);
      draw_text(ctx, font, coord_str, pos_in_screen_space, CHAR_SIZE, color_alpha(COORDINATE_COLOR, (clock_key_held(ctx, KEY_LEFT_ALT) ? 0.75f : 0.2f)));
    }
  }

  // draw pieces
  for (int i = 0; i < arrlenu(pieces); ++i) {
    Piece* p = &pieces[i];
    draw_piece(p);
  }


  // draw selection
  if (selected_piece) {
    Sprite_set_hframe(selected_tile_spr, (is_tile_white(selected_piece->pos) ? 1 : 0));
    draw_sprite_at(ctx, selected_tile_spr, (Vector2f) {(float)selected_piece->pos.x + tile_size/2.f, (float)selected_piece->pos.y + tile_size/2.f});
    /* draw_box(ctx, (Rect) { */
    /* 	  .pos = (Vector2f) {(float)selected_piece->pos.x, (float)selected_piece->pos.y}, */
    /* 	  .size = (Vector2f) {(float)tile_size, (float)tile_size} */
    /* 	}, */
    /*   SELECTED_PIECE_COLOR, */
    /* 	color_alpha(SELECTED_PIECE_COLOR, 0.5f)); */

    Movement_result mr = get_piece_moves(selected_piece);
    if (white_check) {
      filter_moves_that_will_not_protect_king(selected_piece, &mr);
    }
    Vector2i* move_poses = mr.movements;

    draw_hover_piece = false;
    // hovering piece checking
    for (int i = 0; i < arrlenu(move_poses); ++i) {
      Vector2i pos = move_poses[i];
      if (v2i_eq(pos, fix_to_tile_space(clock_mpos_world(ctx)))) {
	draw_hover_piece = true;
	Sprite_set_hframe(hovering_piece_sprite, selected_piece->spr.hframe);
	Vector2i p = fix_to_tile_space(clock_mpos_world(ctx));
	hovering_piece_sprite->pos = (Vector2f) {(float)p.x + tile_size/2.f, (float)p.y + tile_size/2.f};
      }
    }

    for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) {
      Vector2i pos = mr.eatable_piece_ptrs[i]->pos;
      if (v2i_eq(pos, fix_to_tile_space(clock_mpos_world(ctx)))) {
	draw_hover_piece = true;
	Sprite_set_hframe(hovering_piece_sprite, selected_piece->spr.hframe);
	Vector2i p = fix_to_tile_space(clock_mpos_world(ctx));
	hovering_piece_sprite->pos = (Vector2f) {(float)p.x + tile_size/2.f, (float)p.y + tile_size/2.f};
      }
    }

    for (int i = 0; i < arrlenu(move_poses); ++i) {
      Vector2i pos = move_poses[i];

      draw_box(ctx, (Rect) {
	  .pos = (Vector2f) {(float)pos.x, (float)pos.y},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}
	},
	MOVABLE_TILE_COLOR,
	color_alpha(MOVABLE_TILE_COLOR, 0.3f));
    }

    for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) {
      Vector2i pos = mr.eatable_piece_ptrs[i]->pos;

      draw_box(ctx, (Rect) {
	  .pos = (Vector2f) {(float)pos.x, (float)pos.y},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}
	},
	EATABLE_PIECE_COLOR,
	color_alpha(EATABLE_PIECE_COLOR, 0.3f));
    }

    free_movement_results(mr);
  }

  // draw hover piece
  if (draw_hover_piece) {
    draw_sprite(ctx, hovering_piece_sprite);
  }

  // black check
  if (black_check)
    draw_check(ctx, white_checking_piece, black_king);
  if (white_check)
    draw_check(ctx, black_checking_piece, white_king);

#ifdef DEBUG
  {
    Vector2f pos = {0.f, 0.f};
    draw_text(ctx, font, (white_turn ? "White turn" : "Black turn"), pos, CHAR_SIZE, COLOR_WHITE);
    draw_text(ctx, font, (white_turn ? "White turn" : "Black turn"), v2f_subs(pos, 2.f), CHAR_SIZE, COLOR_BLACK);
  }
  // dev_mode indicator
  if (dev_mode) {
    if (black_king) {
      Vector2i pos = black_king->pos;
      draw_box(ctx, (Rect) {
	  .pos = (Vector2f) {(float)pos.x, (float)pos.y},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}
	},
	BLACK_KING_COLOR,
	color_alpha(BLACK_KING_COLOR, 0.3f));
    }

    if (white_king) {
      Vector2i pos = white_king->pos;
      draw_box(ctx, (Rect) {
	  .pos = (Vector2f) {(float)pos.x, (float)pos.y},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}
	},
	WHITE_KING_COLOR,
	color_alpha(WHITE_KING_COLOR, 0.3f));
    }
  }

  clock_use_camera_view(ctx, false);
}

typedef enum {
  STATE_MAINMENU,
  STATE_OPTIONS,
  STATE_PLAY,
  STATE_COUNT,
} State;

static State state = STATE_PLAY;

cstr state_as_str(State s) {
  switch (s) {
  case STATE_MAINMENU: return "Mainmenu";
  case STATE_OPTIONS: return "Options";
  case STATE_PLAY: return "Play";
  default: ASSERT(0 && "Unreachable!");
  }
  return "THIS SHOULDN'T BE VISIBLE!!!";
}

static float a = 0.f;

static void draw_mainmenu_bg(Context* ctx) {
  // draw checker pattern
  for (int y = 0; y < rows; ++y) {
    for (int x = 0; x < cols; ++x) {
      Color col = color_from_hex(0XFFF0F6F0);
      if (y % 2 != x % 2) col = color_from_hex(0xFF232322);
      draw_rect(ctx, (Rect) {
	  .pos = (Vector2f) {(float)x*tile_size, (float)y*tile_size},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}},
	col);
    }
  }

  // draw checker pattern
  set_blend_mode(BLENDMODE_SUB);
  for (int y = 0; y < rows; ++y) {
    for (int x = 0; x < cols; ++x) {
      Color color = color_from_hex(0XFFF0F6F0);
      /* if (y % 2 != x % 2) color = color_from_hex(0xFF232322); */
      /* color = color_alpha(color, 0.4f); */
      draw_rect(ctx, (Rect) {
	  .pos = (Vector2f) {(float)x*tile_size + sinf(a+y)*tile_size*0.5f, (float)y*tile_size + cosf(a+x)*tile_size*0.5f},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}},
	color);
    }
  }
  a += ctx->delta;
  set_blend_mode(BLENDMODE_NORMAL);

  float t = map(sinf(a), -1.f, 1.f, 0.f, 0.75f);

  draw_rect(ctx, (Rect) {
      .pos =  (Vector2f) {0.f, 0.f},
      .size = (Vector2f) {(float)ctx->win->width, (float)ctx->win->height},
    },
    color_alpha(COLOR_BLACK, t));
}

typedef enum Mainmenu_item {
  MAINMENU_ITEM_PLAY,
  MAINMENU_ITEM_OPTIONS,
  MAINMENU_ITEM_QUIT,
  MAINMENU_ITEM_COUNT,
} Mainmenu_item;
static Mainmenu_item selected_mmi = MAINMENU_ITEM_PLAY;

cstr mainmenu_item_as_str(Mainmenu_item mmi) {
  switch (mmi) {
  case MAINMENU_ITEM_PLAY:	{ return "Play"; }
  case MAINMENU_ITEM_OPTIONS:	{ return "Options"; }
  case MAINMENU_ITEM_QUIT:	{ return "Quit"; }
  default: ASSERT(0 && "Unreachable!");
  }
  return "CURSED!";
}

#define MAINMENU_ITEM_STR_COUNT (8 + 4)
char mainmenu_item_str[MAINMENU_ITEM_STR_COUNT] = {0};

static void draw_mainmenu(Context* ctx) {
  draw_mainmenu_bg(ctx);

  float start = (float)ctx->win->height * 0.5f;
  float padding = 0.f;
  int char_size = 72;
  for (int i = 0; i < (int)MAINMENU_ITEM_COUNT; ++i) {
    cstr text = mainmenu_item_as_str((Mainmenu_item)i);
    snprintf(mainmenu_item_str, MAINMENU_ITEM_STR_COUNT, "%s%s%s", (i == (int)selected_mmi ? ">>" : ""), text, (i == (int)selected_mmi ? "<<" : ""));
    Vector2f pos = {10.f, start + ((float)char_size * (float)i) + (padding * (float)i)};
    draw_text(ctx, &ctx->default_font, mainmenu_item_str, pos, char_size, COLOR_WHITE);
  }
}

static void update_mainmenu(Context* ctx) {
  if (clock_key_pressed(ctx, KEY_DOWN)) {
    selected_mmi = (selected_mmi + 1) % MAINMENU_ITEM_COUNT;
  }

  if (clock_key_pressed(ctx, KEY_UP)) {
    selected_mmi--;
    if (selected_mmi < 0) selected_mmi = (int)(MAINMENU_ITEM_COUNT-1);
  }
}

#ifdef DEBUG
int main(void) {
  #else
int WinMain(HINSTANCE instance,
	    HINSTANCE prev_instance,
	    LPSTR     cmd_line,
	    int       show_cmd) {
  (void)instance;
  (void)prev_instance;
  (void)cmd_line;
  (void)show_cmd;
#endif

  srand((uint)time(0));

  Context* ctx = clock_init(960, 960, 1.f, 1.f, "Chess", WINDOW_VSYNC);

  Arena str_arena = Arena_make(0);

  board_rect = (Rect) {
    .pos = {0.f, 0.f},
    .size = {960.f, 960.f},
  };

  if (!ctx) return 1;

  tile_size = (ctx->win->width/cols);

  init_pieces(ctx);

  Font font = ctx->default_font;

#ifdef DEBUG

  UI ui = UI_make(ctx, &font, (Vector2f) {50.f, 10.f}, "Debug");
#endif
  Sprite hovering_piece_sprite = {0};
  if (!Sprite_init(&hovering_piece_sprite, Resman_load_texture_from_file(ctx->resman, "resources/gfx/piece_sheet.png"), PIECE_TYPE_COUNT*2, 1)) return 1;
  Sprite_center_origin(&hovering_piece_sprite);
  hovering_piece_sprite.scale.x = (float)(ctx->win->width/480);
  hovering_piece_sprite.scale.y = (float)(ctx->win->height/480);
  hovering_piece_sprite.tint.a = 0.4f;

  Sprite selected_tile_spr = {0};
  if (!Sprite_init(&selected_tile_spr, Resman_load_texture_from_file(ctx->resman, "resources/gfx/movable_tile.png"), 2, 1)) return 1;
  Sprite_center_origin(&selected_tile_spr);
  selected_tile_spr.scale.x = (float)(ctx->win->width/480)  + 1.f;
  selected_tile_spr.scale.y = (float)(ctx->win->height/480) + 1.f;

  // TEMP: Piece testing
#if TESTING
  {
    Piece p = {0};
    if (!init_piece(&p, ctx, PIECE_TYPE_KING, WHITE)) return 1;
    p.pos = tile_to_screen_space((Vector2i) {4, 4});
    arrput(pieces, p);
  }
#endif

  while (!clock_should_quit(ctx)) {
    clock_begin_draw(ctx);
    clock_clear(ctx, COLOR_BLACK);


    //
    // Draw
    //

    switch (state) {
    case STATE_MAINMENU: {
      draw_mainmenu(ctx);
    } break;
    case STATE_OPTIONS: {
    } break;
    case STATE_PLAY: {
      draw_board(ctx, &font, &selected_tile_spr, &hovering_piece_sprite);
    } break;
    default: ASSERT(0 && "Unreachable!");
    }

#endif
    //
    // Ui
    //

#ifdef DEBUG
    if (dev_mode) {
      UI_begin(&ui, UI_LAYOUT_KIND_VERT);
      Arena_reset(&str_arena);

      cstr state_str = Arena_alloc_str(str_arena, "State: %s", state_as_str(state));
      UI_text(&ui, state_str, 24, COLOR_GOLD);

      /* cstr checked_king_str = Arena_alloc_str(str_arena, "Checked king: %p", checked_king); */
      /* UI_text(&ui, checked_king_str, 24, COLOR_WHITE); */

      cstr undo_cmd_stack_count_str = Arena_alloc_str(str_arena, "Undo stack count: %zu", undo_cmd_stack.count);
      UI_text(&ui, undo_cmd_stack_count_str, 24, COLOR_WHITE);

      Piece** black_piece_ptrs = get_black_piece_ptrs();
      cstr black_pieces_count_str = Arena_alloc_str(str_arena, "Black_pcs count: %zu", arrlenu(black_piece_ptrs));
      UI_text(&ui, black_pieces_count_str, 24, COLOR_WHITE);
      arrfree(black_piece_ptrs);

      Piece** white_piece_ptrs = get_white_piece_ptrs();
      cstr white_pieces_count_str = Arena_alloc_str(str_arena, "White_pcs count: %zu", arrlenu(white_piece_ptrs));
      UI_text(&ui, white_pieces_count_str, 24, COLOR_WHITE);
      arrfree(white_piece_ptrs);

      cstr black_check_str = Arena_alloc_str(str_arena, "black check: %s", (black_check ? "true" : "false"));
      UI_text(&ui, black_check_str, 24, COLOR_WHITE);

      cstr black_checkmate_str = Arena_alloc_str(str_arena, "black checkmate: %s", (black_checkmate ? "true" : "false"));
      UI_text(&ui, black_checkmate_str, 24, COLOR_WHITE);

      cstr white_check_str = Arena_alloc_str(str_arena, "white check: %s", (white_check ? "true" : "false"));
      UI_text(&ui, white_check_str, 24, COLOR_WHITE);

      cstr black_checking_piece_str = Arena_alloc_str(str_arena, "black checking piece: %p", black_checking_piece);
      UI_text(&ui, black_checking_piece_str, 24, COLOR_WHITE);

      cstr white_checking_piece_str = Arena_alloc_str(str_arena, "white checking piece: %p", white_checking_piece);
      UI_text(&ui, white_checking_piece_str, 24, COLOR_WHITE);

      cstr controlling_black_pieces_str = Arena_alloc_str(str_arena, "Controlling all pieces: %s", (user_control_all_pieces ? "true" : "false"));
      UI_text(&ui, controlling_black_pieces_str, 24, COLOR_WHITE);

      cstr ai_control_pieces_str = Arena_alloc_str(str_arena, "AI Control pieces: %s", (ai_control_pieces ? "true" : "false"));
      UI_text(&ui, ai_control_pieces_str, 24, COLOR_WHITE);

      UI_end(&ui);
    }
#endif

    //
    // Update
    //

    if (clock_key_pressed(ctx, KEY_TAB)) {
      state = (state + 1) % STATE_COUNT;
    }

    switch (state) {
    case STATE_MAINMENU: {
      update_mainmenu(ctx);
    } break;
    case STATE_OPTIONS: {
    } break;
    case STATE_PLAY: {
#ifdef DEBUG
      // change turn
      if (clock_key_pressed(ctx, KEY_SPACE)) {
	change_turn();
      }

      // TEMP: camera testing
      const float S = 100.f;
      if (clock_key_held(ctx, KEY_LEFT)) {
	ctx->camera.x -= S * ctx->delta;
      }
      if (clock_key_held(ctx, KEY_RIGHT)) {
	ctx->camera.x += S * ctx->delta;
      }
      if (clock_key_held(ctx, KEY_UP)) {
	ctx->camera.y -= S * ctx->delta;
      }
      if (clock_key_held(ctx, KEY_DOWN)) {
	ctx->camera.y += S * ctx->delta;
      }

      // dev_mode
      if (clock_key_pressed(ctx, KEY_F1)) {
	dev_mode = !dev_mode;
	white_turn = true;
      }

      if (clock_key_pressed(ctx, KEY_F2)) {
	user_control_all_pieces = !user_control_all_pieces;
	white_turn = true;
      }

      if (clock_key_pressed(ctx, KEY_F3)) {
	ai_control_pieces = !ai_control_pieces;
	white_turn = true;
      }
#endif

      // reset
      if (clock_key_held(ctx, KEY_LEFT_CONTROL)) {
	if (clock_key_pressed(ctx, KEY_R)) {
	  init_pieces(ctx);
	}

	// undo
	if (clock_key_pressed(ctx, KEY_Z)) {
	  Undo_cmd undo_cmd = {0};
	  if (Undo_cmd_stack_pop(&undo_cmd_stack, &undo_cmd)) {
	    switch (undo_cmd.type) {
	    case UNDO_CMD_TYPE_MOVE: {
	      undo_cmd.moving_piece->moving = true;
	      undo_cmd.moving_piece->to = undo_cmd.piece_pos;
	      if (undo_cmd.was_first_move) {
		undo_cmd.moving_piece->moved_once = false;
	      }
	    } break;
	    case UNDO_CMD_TYPE_SPAWN: {
	      if (!spawn_piece(ctx, undo_cmd.piece_type, undo_cmd.piece_pos, undo_cmd.piece_black)) {
		// TODO: quit
		return;
	      }
	      correct_undo_move(&pieces[arrlenu(pieces)-1]);
	    } break;
	    case UNDO_CMD_TYPE_REMOVE: {
	    } break;
	    default: ASSERT(0 && "Unreachable!");
	    }
	  }
	}
      }

      // move piece
      if (!animate_piece_moving(((int)ctx->delta) <= 0 ? 1 : (int)ctx->delta)) {
	if (white_turn) {
	  user_control_piece(ctx, user_control_all_pieces);
	} else {
	  ai_control_piece(ctx);
	}
      }

#ifdef DEBUG
      if (dev_mode) {
	// remove piece
	if (clock_mouse_pressed(ctx, MOUSE_BUTTON_MIDDLE)) {
	  select_piece(NULL);
	  Piece* clicked_piece = get_piece_at_pos(fix_to_tile_space(ctx->mpos));
	  if (clicked_piece) {
	    remove_piece(clicked_piece);
	  }
	}
      }
#endif

      white_check = is_piece_in_danger(white_king);

      black_check = is_piece_in_danger(black_king);

      remove_pieces_marked_for_removal();

    } break;
    default: ASSERT(0 && "Unreachable!");
    }


    clock_end_draw(ctx);
  }

  Arena_free(&str_arena);
  Sprite_deinit(&hovering_piece_sprite);
  Sprite_deinit(&selected_tile_spr);
  UI_free(&ui);
  clock_deinit(ctx);

  return 0;
}
