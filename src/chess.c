#include <clock/clock.h>
#include <time.h>

// TODO: Implement Castling
// TODO: Implement Check
// TODO: Implement Checkmate
// NOTE: Check is when king is in eatable but can king can move to safety or another piece can shield the king.
// NOTE: Checkmate is when king /* is in eatable and not the above */

typedef struct Piece Piece;
typedef enum Piece_type Piece_type;

static int tile_size;
static const int cols = 8;
static const int rows = 8;

#define WHITE false
#define BLACK true

#define SELECTED_PIECE_COLOR COLOR_NAVY
#define MOVABLE_TILE_COLOR COLOR_GOLD
#define EATABLE_PIECE_COLOR COLOR_MAGENTA
#define CHECKED_PIECE_COLOR COLOR_RED
#define CHECKING_PIECE_COLOR COLOR_CYAN
#define COORDINATE_COLOR COLOR_GREY

static Piece* pieces = NULL; // dynamic-array
static Piece* selected_piece = NULL;

static Piece* checked_king = NULL;
static Piece* checking_piece = NULL;
static Piece* black_king = NULL;
static Piece* white_king = NULL;

static bool white_turn = true;
static int piece_move_speed = 10;

static bool draw_hover_piece = false;
static bool black_check = false;
static bool black_checkmate = false;
static bool white_check = false;
static bool white_checkmate = false;

// TODO: Move this to clock_vector
static bool v2i_eq(Vector2i a, Vector2i b) {
  return (a.x == b.x) && (a.y == b.y);
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
  Sprite spr;
  Context* ctx;
  bool black;
  bool moved_once;
  bool moving;
};

static void reverse_offsets_y(Vector2i* offsets) {
  for (int i = 0; i < arrlenu(offsets); ++i) {
    offsets[i].y *= -1;
  }
}

Piece* get_piece_at_pos(Vector2i pos) {
  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (v2i_eq(pos, pieces[i].pos)) {
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

#define BREAK_IF_PIECE_EXISTS() \
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
  Piece** eatable_piece_ptrs;
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
	    EAT_PIECE();
	    arrput(offsets, offset);
	  }
	  {
	    offset = (Vector2i) {i*2, j*1};
	    EAT_PIECE();
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
	  EAT_PIECE();
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
      if (existing_piece == NULL)
	arrput(res.movements, pos);
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

void draw_piece(Piece* piece) {
  ASSERT(piece->ctx);

  piece->spr.pos = (Vector2f) {(float)piece->pos.x + tile_size/2.f, (float)piece->pos.y + tile_size/2.f};

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
      if (piece->to.x - piece->pos.x < 0) {
	dir.x = -piece_move_speed;
      } else if (piece->to.x - piece->pos.x > 0) {
	dir.x = piece_move_speed;
      }

      if (piece->to.y - piece->pos.y < 0) {
	dir.y = -piece_move_speed;
      } else if (piece->to.y - piece->pos.y > 0) {
	dir.y = piece_move_speed;
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

// TODO (speed): better way to remove piece
void remove_piece(Piece* piece) {
  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (&pieces[i] == piece) {
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

  Piece* eating_piece = NULL;
  bool remove_eating_piece = false;
  // eat
  for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) {
    eating_piece = mr.eatable_piece_ptrs[i];
    if (v2i_eq(eating_piece->pos, to)) {
      piece->to = to;
      piece->moving = true;
      moved = true;
      remove_eating_piece = true;
      break;
    }
  }

  if (remove_eating_piece &&
      eating_piece) {
    ASSERT(eating_piece != piece);
    if ((*piece_ptr) > eating_piece) {
      (*piece_ptr)--;
      piece = *piece_ptr;
    }

    if (black_king > eating_piece) {
      black_king--;
    }
    if (white_king > eating_piece) {
      white_king--;
    }

    remove_piece(eating_piece);
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
  selected_piece = NULL;
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
	checking_piece = &pieces[i];
	return true;
      }
    }

    free_movement_results(mr);
  }
  checking_piece = NULL;

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
  return arr_count == 1 ? 0 : (rand() % (arr_count-1));
}

void ai_choose_piece(Piece** selecting_piece) {
  Piece** black_piece_ptrs = NULL;

  for (int i = 0; i < arrlenu(pieces); ++i) {
    if (pieces[i].black) {
      arrput(black_piece_ptrs, &pieces[i]);
    }
  }

  ASSERT(arrlenu(black_piece_ptrs) > 0);

  int random_idx = get_random_idx((int)arrlenu(black_piece_ptrs));

  ASSERT(0 <= random_idx && random_idx <= (arrlenu(black_piece_ptrs) - 1));

  *selecting_piece = black_piece_ptrs[random_idx];
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

void ai_control_piece(Context* ctx) {
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
  char* moved_to_coord = get_coord_str(selected_piece->pos);

  log_info("Moved %s %s Piece from %s to %s", (selected_piece->black ? "Black" : "White"), get_piece_type_str(selected_piece->type),
	   moving_from_coord,
	   moved_to_coord);

  free(moving_from_coord);
  free(moved_to_coord);

  selected_piece = NULL;

  if (!selected_piece) {
    white_turn = true;
  }
}

void user_control_piece(Context* ctx, bool can_control_black) {
  if (clock_mouse_pressed(ctx, MOUSE_BUTTON_LEFT)) {
    if (selected_piece) {
      if (move_piece_to(&selected_piece, fix_to_tile_space(ctx->mpos))) {
	selected_piece = NULL;
	white_turn = false;
      }
    } else {
      selected_piece = get_piece_at_pos(fix_to_tile_space(ctx->mpos));
      if (!can_control_black) {
	if (selected_piece && selected_piece->black) selected_piece = NULL;
      }
    }
    draw_hover_piece = false;
  }

  if (clock_mouse_pressed(ctx, MOUSE_BUTTON_RIGHT)) {
    selected_piece = NULL;
    draw_hover_piece = false;
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
  if (!ctx) return 1;

  tile_size = (ctx->win->width/cols);

  init_pieces(ctx);

  Font font = {0};
  if (!Font_init_from_file(&font, ctx, "resources/fonts/WayfarersToyBoxRegular-gxxER.ttf")) {
    return 1;
  }

  bool dev_mode = false;

  UI ui = UI_make(ctx, &font);
  Vector2f ui_pos = {0.f, 0.f};

  Sprite hovering_piece_sprite = {0};
  if (!Sprite_init(&hovering_piece_sprite, Resman_load_texture_from_file(ctx->resman, "resources/gfx/piece_sheet.png"), PIECE_TYPE_COUNT*2, 1)) return 1;

  Sprite_center_origin(&hovering_piece_sprite);

  hovering_piece_sprite.scale.x = (float)(ctx->win->width/480);
  hovering_piece_sprite.scale.y = (float)(ctx->win->height/480);

  hovering_piece_sprite.tint.a = 0.4f;

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
	draw_text(ctx, &font, coord_str, pos_in_screen_space, 24, color_alpha(COORDINATE_COLOR, (clock_key_held(ctx, KEY_LEFT_ALT) ? 0.75f : 0.2f)));
      }
    }

    for (int i = 0; i < arrlenu(pieces); ++i) {
      Piece* p = &pieces[i];
      draw_piece(p);
    }


    // draw selection
    if (selected_piece) {
      draw_box(ctx, (Rect) {
	  .pos = (Vector2f) {(float)selected_piece->pos.x, (float)selected_piece->pos.y},
	  .size = (Vector2f) {(float)tile_size, (float)tile_size}
	},
        SELECTED_PIECE_COLOR,
	color_alpha(SELECTED_PIECE_COLOR, 0.5f));

      Movement_result mr =  get_piece_movement_positions(selected_piece);
      Vector2i* move_poses = mr.movements;

      draw_hover_piece = false;
      // hovering piece checking
      for (int i = 0; i < arrlenu(move_poses); ++i) {
        Vector2i pos = move_poses[i];
	if (v2i_eq(pos, fix_to_tile_space(ctx->mpos))) {
	  draw_hover_piece = true;
	  Sprite_set_hframe(&hovering_piece_sprite, selected_piece->spr.hframe);
	  Vector2i p = fix_to_tile_space(ctx->mpos);
	  hovering_piece_sprite.pos = (Vector2f) {(float)p.x + tile_size/2.f, (float)p.y + tile_size/2.f};;
	}
      }

      for (int i = 0; i < arrlenu(mr.eatable_piece_ptrs); ++i) {
        Vector2i pos = mr.eatable_piece_ptrs[i]->pos;
	if (v2i_eq(pos, fix_to_tile_space(ctx->mpos))) {
	  draw_hover_piece = true;
	  Sprite_set_hframe(&hovering_piece_sprite, selected_piece->spr.hframe);
	  Vector2i p = fix_to_tile_space(ctx->mpos);
	  hovering_piece_sprite.pos = (Vector2f) {(float)p.x + tile_size/2.f, (float)p.y + tile_size/2.f};;
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
      draw_sprite(ctx, &hovering_piece_sprite);
    }

    // check
    if (checked_king && checking_piece) {

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

    // dev_mode indicator
    if (dev_mode) {
      draw_text(ctx, &font, "Dev_mode", (Vector2f) {0.f, 0.f}, 12, COLOR_RED);
    }

    //
    // Ui
    //

#ifdef DEBUG
    UI_begin(&ui, &ui_pos, UI_LAYOUT_KIND_VERT);

    cstr black_check_str = "";
    temp_sprintf(black_check_str, "Black check: %s", (black_check ? "true" : "false"));
    UI_text(&ui, black_check_str, 24, COLOR_WHITE);

    cstr white_check_str = "";
    temp_sprintf(white_check_str, "White check: %s", (white_check ? "true" : "false"));
    UI_text(&ui, white_check_str, 24, COLOR_WHITE);


    UI_end(&ui);
#endif

    //
    // Update
    //

    // dev_mode
    if (clock_key_pressed(ctx, KEY_F1)) {
      dev_mode = !dev_mode;
      white_turn = true;
    }

    // reset
    if (clock_key_pressed(ctx, KEY_R)) {
      init_pieces(ctx);
    }

    // move piece
    if (dev_mode) {
      if (!animate_piece_moving(((int)ctx->delta) <= 0 ? 1 : (int)ctx->delta)) {
	user_control_piece(ctx, true);
      }

      // remove piece
      if (clock_mouse_pressed(ctx, MOUSE_BUTTON_MIDDLE)) {
	selected_piece = NULL;
	Piece* clicked_piece = get_piece_at_pos(fix_to_tile_space(ctx->mpos));
	if (clicked_piece) {
	  remove_piece(clicked_piece);
	}
      }
    } else {
      if (!animate_piece_moving(((int)ctx->delta) <= 0 ? 1 : (int)ctx->delta)) {
	if (white_turn) {
	  white_check = is_piece_in_danger(white_king);
	  if (white_check) { checked_king = white_king; }
	  user_control_piece(ctx, false);
	} else {
	  ai_control_piece(ctx);
	  black_check = is_piece_in_danger(black_king);
	  if (black_check) { checked_king = black_king; }
	 }
      }
    }

    clock_end_draw(ctx);
  }

  Font_deinit(&font);
  clock_deinit(ctx);

  return 0;
}
