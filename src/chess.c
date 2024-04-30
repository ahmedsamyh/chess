#include <clock/clock.h>
#include <time.h>

// TODO: Implement Castling
// TODO: Implement Checkmate
// TODO: Implement Check
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


static void change_turn(void) {
  white_turn = !white_turn;
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
Movement_result get_piece_movement_positions(Piece* piece) {
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

  for (int i = 0; i < arrlenu(offsets); ++i) {
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
      return true;
    }
  }
  return false;
}

inline void offset_piece_when_removing_piece(Piece** piece, Piece* removing_piece) {
  if ((*piece) > removing_piece) {
    (*piece)--;
  }
}

// TODO (speed): better way to remove piece
void remove_piece(Piece* removing_piece) {
  offset_piece_when_removing_piece(&black_king, removing_piece);
  offset_piece_when_removing_piece(&white_king, removing_piece);
  if (white_checking_piece) offset_piece_when_removing_piece(&white_checking_piece, removing_piece);
  if (black_checking_piece) offset_piece_when_removing_piece(&black_checking_piece, removing_piece);

  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (&pieces[i] == removing_piece) {
      deinit_piece(&pieces[i]);
      arrdel(pieces, i);
    }
  }
}

bool move_piece_to(Piece** piece_ptr, Vector2i to) {
  Piece* piece = *piece_ptr;
  if (!piece) return false;
  Movement_result mr = get_piece_movement_positions(piece);
  Vector2i* move_poses = mr.movements;

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
      break;
    }
  }

  if (remove_eated_piece &&
      eated_piece) {
    ASSERT(eated_piece != piece);

    Piece_removal_entry entry =  {
      .eating_piece = piece,
      .eated_piece = eated_piece
    };
    ASSERT(entry.eating_piece->black != entry.eated_piece->black);
    arrput(piece_removal_entries, entry);
  }

  // move
  for (int i = 0; i < arrlenu(move_poses); ++i) {
    Vector2i pos = move_poses[i];
    if (v2i_eq(pos, to)) {
      piece->to = to;
      /* piece->pos = to; */
      piece->moving = true;
      moved = true;
      break;
    }
  }

  free_movement_results(mr);

  if (!piece->moved_once && moved) {
    piece->moved_once = true;
  }

  return moved;
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
  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (pieces[i].black == piece->black) continue;

    Movement_result mr = get_piece_movement_positions(&pieces[i]);

    for (int j = 0; j < arrlenu(mr.eatable_piece_ptrs); ++j) {
      if (mr.eatable_piece_ptrs[j] == piece) {
	if (piece->black) {
	  white_checking_piece = &pieces[i];
	} else {
	  black_checking_piece = &pieces[i];
	}
	return true;
      }
    }

    free_movement_results(mr);
  }
  white_checking_piece = NULL;
  black_checking_piece = NULL;

  return false;
}

/* bool move_is_check(Piece* piece, Piece** checked_piece) { */
/*   Movement_result mr = get_piece_movement_positions(piece); */

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
      move_piece_to(&selecting_piece, random_move);
    } else {
      int random_idx = get_random_idx(eatable_piece_ptrs_count);
      Vector2i random_move = mr.eatable_piece_ptrs[random_idx]->pos;
      move_piece_to(&selecting_piece, random_move);
    }
    return;
  }

  if (movements_count > 0) {
    int random_idx = get_random_idx(movements_count);
    Vector2i random_move = mr.movements[random_idx];
    move_piece_to(&selecting_piece, random_move);
    return;
  }

  if (eatable_piece_ptrs_count > 0) {
    int random_idx = get_random_idx(eatable_piece_ptrs_count);
    Vector2i random_move = mr.eatable_piece_ptrs[random_idx]->pos;
    move_piece_to(&selecting_piece, random_move);
    return;
  }

  ASSERT(0 && "Unreachable!");
}

/* // TODO: Optimize */

// ## Ways to get out of Check
// -> king can move out of danger
// -> other piece can protect king
// -> any piece can eat the checking piece

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
  Movement_result mr = get_piece_movement_positions(black_king);

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
    move_piece_to(&black_king, mr.movements[random_idx]);

    if (is_piece_in_danger(black_king)) {
      black_king->to = prev_pos;
      arrput(tried_indices, random_idx);
      goto checked_king_move_out_of_danger_retry;
    }

    arrfree(tried_indices);
  }

  free_movement_results(mr);

  return true;
}

inline bool does_piece_movements_collide(Piece* a, Piece* b, Vector2i* colliding_pos) {
    Movement_result a_mr = get_piece_movement_positions(a);
    Movement_result b_mr = get_piece_movement_positions(b);

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
    Movement_result mr = get_piece_movement_positions(p);

    size_t eatable_piece_ptrs_count = arrlenu(mr.eatable_piece_ptrs);
    for (int j = 0; j < eatable_piece_ptrs_count; ++j) {
      if (mr.eatable_piece_ptrs[j] == white_checking_piece) {
	eating_piece = p;
	p->to = white_checking_piece->pos;
	p->moving = true;
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

    return;
  }

  (void)ctx;
  Movement_result mr = {0};

 ai_rechoose_move:
  while (arrlenu(mr.movements) == 0 && arrlenu(mr.eatable_piece_ptrs) == 0) {
    ai_choose_piece(&selected_piece);

    free_movement_results(mr);

    mr = get_piece_movement_positions(selected_piece);
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

void user_control_piece(Context* ctx, bool can_control_black) {
  if (white_check) {

    return;
  }

  if (clock_mouse_pressed(ctx, MOUSE_BUTTON_LEFT)) {
    if (selected_piece) {
      if (move_piece_to(&selected_piece, fix_to_tile_space(ctx->mpos))) {
	select_piece(NULL);
	change_turn();
      }
    } else {
      select_piece(get_piece_at_pos(fix_to_tile_space(ctx->mpos)));
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
  ASSERT(checking_piece);
  ASSERT(checked_king);
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

    Movement_result mr =  get_piece_movement_positions(selected_piece);
    Vector2i* move_poses = mr.movements;

    draw_hover_piece = false;
    // hovering piece checking
    for (int i = 0; i < arrlenu(move_poses); ++i) {
      Vector2i pos = move_poses[i];
      if (v2i_eq(pos, fix_to_tile_space(clock_mpos_world(ctx)))) {
	draw_hover_piece = true;
	Sprite_set_hframe(hovering_piece_sprite, selected_piece->spr.hframe);
	Vector2i p = fix_to_tile_space(clock_mpos_world(ctx));
	hovering_piece_sprite->pos = (Vector2f) {(float)p.x + tile_size/2.f, (float)p.y + tile_size/2.f};;
      }
    }

    for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) {
      Vector2i pos = mr.eatable_piece_ptrs[i]->pos;
      if (v2i_eq(pos, fix_to_tile_space(clock_mpos_world(ctx)))) {
	draw_hover_piece = true;
	Sprite_set_hframe(hovering_piece_sprite, selected_piece->spr.hframe);
	Vector2i p = fix_to_tile_space(clock_mpos_world(ctx));
	hovering_piece_sprite->pos = (Vector2f) {(float)p.x + tile_size/2.f, (float)p.y + tile_size/2.f};;
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

      Vector2f text_pos = v2i_to_v2f(v2i_add(pos, (Vector2i) {tile_size, tile_size}));

      draw_text(ctx, font, "Black King", v2f_subs(text_pos, 2.f), 18, COLOR_BLACK);
      draw_text(ctx, font, "Black King", text_pos, 18, COLOR_WHITE);

      draw_imm_line(ctx, text_pos, v2i_to_v2f(pos), COLOR_WHITE, COLOR_WHITE);
    }

    if (white_king) {
      Vector2i pos = white_king->pos;
      draw_box(ctx, (Rect) {
	  .pos = (Vector2f) {(float)pos.x, (float)pos.y},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}
	},
	WHITE_KING_COLOR,
	color_alpha(WHITE_KING_COLOR, 0.3f));
      Vector2f text_pos = v2i_to_v2f(v2i_sub(pos, (Vector2i) {tile_size, tile_size}));

      draw_text(ctx, font, "White King", v2f_subs(text_pos, 2.f), 18, COLOR_BLACK);
      draw_text(ctx, font, "White King", text_pos, 18, COLOR_WHITE);

      draw_imm_line(ctx, text_pos, v2i_to_v2f(pos), COLOR_WHITE, COLOR_WHITE);

    }
  }

  clock_use_camera_view(ctx, false);
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

  UI ui = UI_make(ctx, &font, (Vector2f) {0.f, 0.f}, "Debug");
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

    draw_board(ctx, &font, &selected_tile_spr, &hovering_piece_sprite);

#endif
    //
    // Ui
    //

#ifdef DEBUG
    if (dev_mode) {
      UI_begin(&ui, UI_LAYOUT_KIND_VERT);

      /* cstr checked_king_str = Arena_alloc_str(str_arena, "Checked king: %p", checked_king); */
      /* UI_text(&ui, checked_king_str, 24, COLOR_GOLD); */

      Arena_reset(&str_arena);

      Piece** black_piece_ptrs = get_black_piece_ptrs();
      cstr black_pieces_count_str = Arena_alloc_str(str_arena, "Black_pcs count: %zu", arrlenu(black_piece_ptrs));
      UI_text(&ui, black_pieces_count_str, 24, COLOR_GOLD);
      arrfree(black_piece_ptrs);

      Piece** white_piece_ptrs = get_white_piece_ptrs();
      cstr white_pieces_count_str = Arena_alloc_str(str_arena, "White_pcs count: %zu", arrlenu(white_piece_ptrs));
      UI_text(&ui, white_pieces_count_str, 24, COLOR_GOLD);
      arrfree(white_piece_ptrs);

      /* cstr black_check_str = Arena_alloc_str(str_arena, "black check: %s", (black_check ? "true" : "false")); */
      /* UI_text(&ui, black_check_str, 24, COLOR_GOLD); */

      /* cstr white_check_str = Arena_alloc_str(str_arena, "white check: %s", (white_check ? "true" : "false")); */
      /* UI_text(&ui, white_check_str, 24, COLOR_GOLD); */

      cstr black_checking_piece_str = Arena_alloc_str(str_arena, "black checking piece: %p", black_checking_piece);
      UI_text(&ui, black_checking_piece_str, 24, COLOR_GOLD);

      cstr white_checking_piece_str = Arena_alloc_str(str_arena, "white checking piece: %p", white_checking_piece);
      UI_text(&ui, white_checking_piece_str, 24, COLOR_GOLD);

      cstr controlling_black_pieces_str = Arena_alloc_str(str_arena, "Controlling all pieces: %s", (user_control_all_pieces ? "true" : "false"));
      UI_text(&ui, controlling_black_pieces_str, 24, COLOR_GOLD);

      UI_end(&ui);
    }
#endif

    //
    // Update
    //
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
#endif

    // reset
    if (clock_key_pressed(ctx, KEY_R)) {
      init_pieces(ctx);
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

    clock_end_draw(ctx);
  }

  Arena_free(&str_arena);
  Sprite_deinit(&hovering_piece_sprite);
  Sprite_deinit(&selected_tile_spr);
  UI_free(&ui);
  clock_deinit(ctx);

  return 0;
}
