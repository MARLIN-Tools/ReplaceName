#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <limits>
#include <mutex>
#include <optional>
#include <random>
#include <sstream>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

static constexpr const char* kEngineName = u8R"ENGNAME(the greatest engine ever!!!!!! sponsored by me and my dad, and I worked so hard on this engine so it is super strong and fast and smart and will win every game!!!!! except the ones it doesn’t because of those leelers scraping unoriginal data es-em-aytch /j anyways I’ve been sick the past few weeks and it sucks a lot but now I have the time to make this super cool engine which you are not reading the full name of for some reason!!! if you have gotten this far in the engine name thank you so much I can’t imagine many have gotten to here. bananas! this engine has all the bells and whistles like alpha beta search wait a second what about gamma search bc like gamma rays are way more dangerous it would probably beat alpha zero which is the best chess engine still in 2026!!! it is 5000 elo and nothing comes close. it will probably invent a new opening because it’s so cool and in advance I will call it the Ohio attack 🗿 I bet you didn’t know emojis could even go in engine names well they CAN it’s a novelty with a lowercase n. speaking of novelties, shashin theory is like the next step in chess evolution. it will align all the atoms of the universe together to paint a beautiful picture of the solution of chess, like the infinite monkeys in the library typing Shakespeare. YOU WILL BE AMAZED IT WILL WIN ALL YOUR TOURNAMENTS EXCEPT THE RIGGED ONES! chatgpt generate the logo to be a giant walrus riding a unicycle and holding a banner saying “I defeated Stockfish” at the beach while a stockfish fish is crying in the background. that’s graphic design for you. graphic design is my passion. today is February 24, 2026. why are you still reading this I did not think anyone would put up with this nonsense for this long but here you and I are. ok fine I give up but before I end this charade I must do two things, which are first that i lied i did not work hard on this at all it was vibe coded in 30 minutes by just telling chatgpt codex make this engine as strong as possible and two i must give you the version number which is v0.123456789(number which is 10 but in one character)(number which is 11 but in one character) pro plus premium edition X)ENGNAME";
static constexpr const char* kEngineAuthor = "Codex";

using U64 = std::uint64_t;
using Clock = std::chrono::steady_clock;

static constexpr int INF = 32000;
static constexpr int MATE_SCORE = 31000;
static constexpr int MAX_PLY = 128;
static constexpr int MAX_MOVES = 256;

enum Color : std::uint8_t { WHITE = 0, BLACK = 1, COLOR_NB = 2 };
enum PieceType : std::uint8_t { PAWN = 0, KNIGHT = 1, BISHOP = 2, ROOK = 3, QUEEN = 4, KING = 5, PIECE_TYPE_NB = 6 };
enum Piece : std::uint8_t {
    EMPTY = 0,
    W_PAWN = 1,
    W_KNIGHT = 2,
    W_BISHOP = 3,
    W_ROOK = 4,
    W_QUEEN = 5,
    W_KING = 6,
    B_PAWN = 7,
    B_KNIGHT = 8,
    B_BISHOP = 9,
    B_ROOK = 10,
    B_QUEEN = 11,
    B_KING = 12
};

enum Castling : std::uint8_t {
    CASTLE_WHITE_OO = 1,
    CASTLE_WHITE_OOO = 2,
    CASTLE_BLACK_OO = 4,
    CASTLE_BLACK_OOO = 8
};

enum MoveFlags : std::uint8_t {
    FLAG_NONE = 0,
    FLAG_CAPTURE = 1,
    FLAG_EN_PASSANT = 2,
    FLAG_CASTLE = 4,
    FLAG_DOUBLE_PUSH = 8
};

using Move = std::uint32_t;

inline int piece_color(Piece p) { return p >= B_PAWN ? BLACK : WHITE; }
inline int piece_type(Piece p) {
    if (p == EMPTY) return -1;
    return (int(p) - 1) % 6;
}
inline Piece make_piece(int c, int pt) {
    return Piece(1 + pt + 6 * c);
}

inline Move make_move(int from, int to, int promo = 0, int flags = 0) {
    return Move((from & 63) | ((to & 63) << 6) | ((promo & 7) << 12) | ((flags & 15) << 15));
}
inline int move_from(Move m) { return int(m & 63); }
inline int move_to(Move m) { return int((m >> 6) & 63); }
inline int move_promo(Move m) { return int((m >> 12) & 7); }
inline int move_flags(Move m) { return int((m >> 15) & 15); }

inline U64 bit(int sq) { return 1ULL << sq; }
inline int file_of(int sq) { return sq & 7; }
inline int rank_of(int sq) { return sq >> 3; }
inline int pop_lsb(U64& b) {
#if defined(_MSC_VER)
    unsigned long idx;
    _BitScanForward64(&idx, b);
    b &= b - 1;
    return int(idx);
#else
    int idx = __builtin_ctzll(b);
    b &= b - 1;
    return idx;
#endif
}
inline int popcount(U64 b) {
#if defined(_MSC_VER)
    return int(__popcnt64(b));
#else
    return __builtin_popcountll(b);
#endif
}

inline std::string square_to_string(int sq) {
    std::string s = "a1";
    s[0] = char('a' + file_of(sq));
    s[1] = char('1' + rank_of(sq));
    return s;
}

inline int string_to_square(const std::string& s) {
    if (s.size() < 2) return -1;
    char f = char(std::tolower((unsigned char) s[0]));
    char r = s[1];
    if (f < 'a' || f > 'h' || r < '1' || r > '8') return -1;
    return (r - '1') * 8 + (f - 'a');
}

std::array<U64, 64> g_knightAtt{};
std::array<U64, 64> g_kingAtt{};
std::array<std::array<U64, 64>, 2> g_pawnAtt{};
std::array<U64, 8> g_fileMask{};
std::array<U64, 8> g_rankMask{};
std::array<U64, 64> g_bbSquare{};
std::array<U64, 8> g_adjacentFileMask{};
std::array<std::array<U64, 64>, 2> g_passedMask{};

U64 ray_attacks(int sq, int dr, int df, U64 occ) {
    U64 a = 0;
    int r = rank_of(sq);
    int f = file_of(sq);
    r += dr;
    f += df;
    while (r >= 0 && r < 8 && f >= 0 && f < 8) {
        int to = r * 8 + f;
        a |= bit(to);
        if (occ & bit(to)) break;
        r += dr;
        f += df;
    }
    return a;
}

inline U64 bishop_attacks(int sq, U64 occ) {
    return ray_attacks(sq, 1, 1, occ) | ray_attacks(sq, 1, -1, occ) | ray_attacks(sq, -1, 1, occ)
         | ray_attacks(sq, -1, -1, occ);
}
inline U64 rook_attacks(int sq, U64 occ) {
    return ray_attacks(sq, 1, 0, occ) | ray_attacks(sq, -1, 0, occ) | ray_attacks(sq, 0, 1, occ)
         | ray_attacks(sq, 0, -1, occ);
}

void init_attack_tables() {
    for (int sq = 0; sq < 64; ++sq) {
        g_bbSquare[sq] = bit(sq);
    }
    for (int f = 0; f < 8; ++f) {
        U64 fm = 0;
        for (int r = 0; r < 8; ++r) fm |= bit(r * 8 + f);
        g_fileMask[f] = fm;
    }
    for (int f = 0; f < 8; ++f) {
        U64 am = 0;
        if (f > 0) am |= g_fileMask[f - 1];
        if (f < 7) am |= g_fileMask[f + 1];
        g_adjacentFileMask[f] = am;
    }
    for (int r = 0; r < 8; ++r) {
        U64 rm = 0;
        for (int f = 0; f < 8; ++f) rm |= bit(r * 8 + f);
        g_rankMask[r] = rm;
    }

    static const int kKnightDelta[8][2] = {
      {1, 2}, {2, 1}, {2, -1}, {1, -2}, {-1, -2}, {-2, -1}, {-2, 1}, {-1, 2}
    };
    static const int kKingDelta[8][2] = {
      {1, 1}, {1, 0}, {1, -1}, {0, 1}, {0, -1}, {-1, 1}, {-1, 0}, {-1, -1}
    };
    for (int sq = 0; sq < 64; ++sq) {
        int r = rank_of(sq);
        int f = file_of(sq);
        U64 kAtt = 0, nAtt = 0, pW = 0, pB = 0;
        for (const auto& d : kKnightDelta) {
            int rr = r + d[0], ff = f + d[1];
            if (rr >= 0 && rr < 8 && ff >= 0 && ff < 8) nAtt |= bit(rr * 8 + ff);
        }
        for (const auto& d : kKingDelta) {
            int rr = r + d[0], ff = f + d[1];
            if (rr >= 0 && rr < 8 && ff >= 0 && ff < 8) kAtt |= bit(rr * 8 + ff);
        }
        if (r < 7 && f > 0) pW |= bit((r + 1) * 8 + (f - 1));
        if (r < 7 && f < 7) pW |= bit((r + 1) * 8 + (f + 1));
        if (r > 0 && f > 0) pB |= bit((r - 1) * 8 + (f - 1));
        if (r > 0 && f < 7) pB |= bit((r - 1) * 8 + (f + 1));

        U64 pMaskW = 0, pMaskB = 0;
        for (int rr = r + 1; rr < 8; ++rr)
            for (int ff = std::max(0, f - 1); ff <= std::min(7, f + 1); ++ff) pMaskW |= bit(rr * 8 + ff);
        for (int rr = r - 1; rr >= 0; --rr)
            for (int ff = std::max(0, f - 1); ff <= std::min(7, f + 1); ++ff) pMaskB |= bit(rr * 8 + ff);

        g_knightAtt[sq] = nAtt;
        g_kingAtt[sq] = kAtt;
        g_pawnAtt[WHITE][sq] = pW;
        g_pawnAtt[BLACK][sq] = pB;
        g_passedMask[WHITE][sq] = pMaskW;
        g_passedMask[BLACK][sq] = pMaskB;
    }
}

static std::array<std::array<U64, 64>, 13> z_piece{};
static std::array<U64, 16> z_castle{};
static std::array<U64, 9> z_ep{};
static U64 z_side = 0;

U64 splitmix64(U64& x) {
    U64 z = (x += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

void init_zobrist() {
    U64 seed = 0xA1B2C3D4E5F60789ULL;
    for (int p = 0; p < 13; ++p)
        for (int sq = 0; sq < 64; ++sq) z_piece[p][sq] = splitmix64(seed);
    for (int i = 0; i < 16; ++i) z_castle[i] = splitmix64(seed);
    for (int i = 0; i < 9; ++i) z_ep[i] = splitmix64(seed);
    z_side = splitmix64(seed);
}

struct Undo {
    int castlingRights = 0;
    int epSquare = -1;
    int rule50 = 0;
    int fullmove = 1;
    Piece captured = EMPTY;
    U64 key = 0;
};

struct MoveList {
    std::array<Move, MAX_MOVES> moves{};
    int size = 0;
    void add(Move m) {
        if (size < MAX_MOVES) moves[size++] = m;
    }
};

struct Position {
    std::array<Piece, 64> board{};
    std::array<std::array<U64, PIECE_TYPE_NB>, COLOR_NB> bb{};
    std::array<U64, COLOR_NB> occ{};
    U64 occAll = 0;
    std::array<int, COLOR_NB> kingSq{4, 60};
    int sideToMove = WHITE;
    int castlingRights = CASTLE_WHITE_OO | CASTLE_WHITE_OOO | CASTLE_BLACK_OO | CASTLE_BLACK_OOO;
    int epSquare = -1;
    int rule50 = 0;
    int fullmove = 1;
    U64 key = 0;
    std::vector<U64> repKeys;

    void clear() {
        board.fill(EMPTY);
        for (auto& c : bb) c.fill(0);
        occ.fill(0);
        occAll = 0;
        kingSq = {4, 60};
        sideToMove = WHITE;
        castlingRights = 0;
        epSquare = -1;
        rule50 = 0;
        fullmove = 1;
        key = 0;
        repKeys.clear();
        if (repKeys.capacity() < 1024) repKeys.reserve(1024);
    }

    U64 compute_key() const {
        U64 k = 0;
        for (int sq = 0; sq < 64; ++sq) {
            Piece p = board[sq];
            if (p != EMPTY) k ^= z_piece[p][sq];
        }
        k ^= z_castle[castlingRights & 15];
        if (epSquare != -1) k ^= z_ep[file_of(epSquare)];
        else k ^= z_ep[8];
        if (sideToMove == BLACK) k ^= z_side;
        return k;
    }

    void place_piece(int sq, Piece p) {
        board[sq] = p;
        if (p == EMPTY) return;
        int c = piece_color(p);
        int pt = piece_type(p);
        bb[c][pt] |= bit(sq);
        occ[c] |= bit(sq);
        occAll |= bit(sq);
        if (pt == KING) kingSq[c] = sq;
    }

    Piece remove_piece(int sq) {
        Piece p = board[sq];
        if (p == EMPTY) return EMPTY;
        int c = piece_color(p);
        int pt = piece_type(p);
        bb[c][pt] &= ~bit(sq);
        occ[c] &= ~bit(sq);
        occAll &= ~bit(sq);
        board[sq] = EMPTY;
        return p;
    }

    void move_piece_raw(int from, int to) {
        Piece p = remove_piece(from);
        place_piece(to, p);
    }

    bool set_fen(const std::string& fen) {
        clear();
        std::istringstream ss(fen);
        std::string boardPart, sidePart, castlePart, epPart;
        if (!(ss >> boardPart >> sidePart >> castlePart >> epPart >> rule50 >> fullmove)) return false;

        int sq = 56;
        for (char ch : boardPart) {
            if (ch == '/') {
                sq -= 16;
                continue;
            }
            if (std::isdigit((unsigned char) ch)) {
                sq += ch - '0';
                continue;
            }
            Piece p = EMPTY;
            switch (ch) {
            case 'P': p = W_PAWN; break;
            case 'N': p = W_KNIGHT; break;
            case 'B': p = W_BISHOP; break;
            case 'R': p = W_ROOK; break;
            case 'Q': p = W_QUEEN; break;
            case 'K': p = W_KING; break;
            case 'p': p = B_PAWN; break;
            case 'n': p = B_KNIGHT; break;
            case 'b': p = B_BISHOP; break;
            case 'r': p = B_ROOK; break;
            case 'q': p = B_QUEEN; break;
            case 'k': p = B_KING; break;
            default: return false;
            }
            if (sq < 0 || sq >= 64) return false;
            place_piece(sq, p);
            ++sq;
        }

        sideToMove = (sidePart == "w") ? WHITE : BLACK;
        castlingRights = 0;
        if (castlePart.find('K') != std::string::npos) castlingRights |= CASTLE_WHITE_OO;
        if (castlePart.find('Q') != std::string::npos) castlingRights |= CASTLE_WHITE_OOO;
        if (castlePart.find('k') != std::string::npos) castlingRights |= CASTLE_BLACK_OO;
        if (castlePart.find('q') != std::string::npos) castlingRights |= CASTLE_BLACK_OOO;
        epSquare = (epPart == "-") ? -1 : string_to_square(epPart);

        key = compute_key();
        repKeys.push_back(key);
        return true;
    }

    bool set_startpos() {
        return set_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    }

    bool square_attacked(int sq, int byColor) const {
        if (byColor == WHITE) {
            if (file_of(sq) > 0 && sq >= 9 && board[sq - 9] == W_PAWN) return true;
            if (file_of(sq) < 7 && sq >= 7 && board[sq - 7] == W_PAWN) return true;
        } else {
            if (file_of(sq) > 0 && sq <= 56 && board[sq + 7] == B_PAWN) return true;
            if (file_of(sq) < 7 && sq <= 54 && board[sq + 9] == B_PAWN) return true;
        }

        if (g_knightAtt[sq] & bb[byColor][KNIGHT]) return true;
        if (g_kingAtt[sq] & bb[byColor][KING]) return true;

        U64 bishops = bb[byColor][BISHOP] | bb[byColor][QUEEN];
        U64 rooks = bb[byColor][ROOK] | bb[byColor][QUEEN];
        if (bishop_attacks(sq, occAll) & bishops) return true;
        if (rook_attacks(sq, occAll) & rooks) return true;
        return false;
    }

    bool in_check(int c) const {
        return square_attacked(kingSq[c], c ^ 1);
    }

    bool has_non_pawn_material(int c) const {
        return (bb[c][KNIGHT] | bb[c][BISHOP] | bb[c][ROOK] | bb[c][QUEEN]) != 0;
    }

    bool is_capture_move(Move m) const {
        if (move_flags(m) & FLAG_EN_PASSANT) return true;
        Piece p = board[move_to(m)];
        return p != EMPTY;
    }

    bool do_move(Move m, Undo& u) {
        int from = move_from(m);
        int to = move_to(m);
        int flags = move_flags(m);
        int promo = move_promo(m);
        Piece pc = board[from];
        if (pc == EMPTY) return false;
        int us = sideToMove;

        u.castlingRights = castlingRights;
        u.epSquare = epSquare;
        u.rule50 = rule50;
        u.fullmove = fullmove;
        u.key = key;
        u.captured = EMPTY;

        key ^= z_castle[castlingRights & 15];
        key ^= z_ep[(epSquare != -1) ? file_of(epSquare) : 8];

        ++rule50;
        epSquare = -1;

        if (flags & FLAG_EN_PASSANT) {
            int capSq = (us == WHITE) ? (to - 8) : (to + 8);
            u.captured = remove_piece(capSq);
            if (u.captured != EMPTY) key ^= z_piece[u.captured][capSq];
        } else if (flags & FLAG_CAPTURE) {
            u.captured = remove_piece(to);
            if (u.captured != EMPTY) key ^= z_piece[u.captured][to];
        }

        if (u.captured != EMPTY || piece_type(pc) == PAWN) rule50 = 0;

        remove_piece(from);
        key ^= z_piece[pc][from];
        Piece moved = pc;
        if (promo && piece_type(pc) == PAWN) {
            int pt = QUEEN;
            if (promo == 1) pt = KNIGHT;
            else if (promo == 2) pt = BISHOP;
            else if (promo == 3) pt = ROOK;
            moved = make_piece(us, pt);
        }
        place_piece(to, moved);
        key ^= z_piece[moved][to];

        if (flags & FLAG_CASTLE) {
            if (to == 6) {
                move_piece_raw(7, 5);
                key ^= z_piece[W_ROOK][7] ^ z_piece[W_ROOK][5];
            } else if (to == 2) {
                move_piece_raw(0, 3);
                key ^= z_piece[W_ROOK][0] ^ z_piece[W_ROOK][3];
            } else if (to == 62) {
                move_piece_raw(63, 61);
                key ^= z_piece[B_ROOK][63] ^ z_piece[B_ROOK][61];
            } else if (to == 58) {
                move_piece_raw(56, 59);
                key ^= z_piece[B_ROOK][56] ^ z_piece[B_ROOK][59];
            }
        }

        if ((flags & FLAG_DOUBLE_PUSH) && piece_type(pc) == PAWN) epSquare = (from + to) / 2;

        if (piece_type(pc) == KING) {
            if (us == WHITE) castlingRights &= ~(CASTLE_WHITE_OO | CASTLE_WHITE_OOO);
            else castlingRights &= ~(CASTLE_BLACK_OO | CASTLE_BLACK_OOO);
        }
        if (piece_type(pc) == ROOK) {
            if (from == 0) castlingRights &= ~CASTLE_WHITE_OOO;
            else if (from == 7) castlingRights &= ~CASTLE_WHITE_OO;
            else if (from == 56) castlingRights &= ~CASTLE_BLACK_OOO;
            else if (from == 63) castlingRights &= ~CASTLE_BLACK_OO;
        }
        if (u.captured == W_ROOK) {
            if (to == 0) castlingRights &= ~CASTLE_WHITE_OOO;
            else if (to == 7) castlingRights &= ~CASTLE_WHITE_OO;
        } else if (u.captured == B_ROOK) {
            if (to == 56) castlingRights &= ~CASTLE_BLACK_OOO;
            else if (to == 63) castlingRights &= ~CASTLE_BLACK_OO;
        }

        if (us == BLACK) ++fullmove;
        sideToMove ^= 1;
        key ^= z_castle[castlingRights & 15];
        key ^= z_ep[(epSquare != -1) ? file_of(epSquare) : 8];
        key ^= z_side;
        repKeys.push_back(key);
        return true;
    }

    void undo_move(Move m, const Undo& u) {
        repKeys.pop_back();
        sideToMove ^= 1;
        int us = sideToMove;
        int from = move_from(m);
        int to = move_to(m);
        int flags = move_flags(m);
        int promo = move_promo(m);

        if (flags & FLAG_CASTLE) {
            if (to == 6) move_piece_raw(5, 7);
            else if (to == 2) move_piece_raw(3, 0);
            else if (to == 62) move_piece_raw(61, 63);
            else if (to == 58) move_piece_raw(59, 56);
        }

        Piece moved = remove_piece(to);
        if (promo) moved = make_piece(us, PAWN);
        place_piece(from, moved);

        if (flags & FLAG_EN_PASSANT) {
            int capSq = (us == WHITE) ? (to - 8) : (to + 8);
            if (u.captured != EMPTY) place_piece(capSq, u.captured);
        } else if (u.captured != EMPTY) {
            place_piece(to, u.captured);
        }

        castlingRights = u.castlingRights;
        epSquare = u.epSquare;
        rule50 = u.rule50;
        fullmove = u.fullmove;
        key = u.key;
    }

    void do_null(Undo& u) {
        u.castlingRights = castlingRights;
        u.epSquare = epSquare;
        u.rule50 = rule50;
        u.fullmove = fullmove;
        u.captured = EMPTY;
        u.key = key;

        key ^= z_ep[(epSquare != -1) ? file_of(epSquare) : 8];
        ++rule50;
        epSquare = -1;
        key ^= z_ep[8];
        if (sideToMove == BLACK) ++fullmove;
        sideToMove ^= 1;
        key ^= z_side;
        repKeys.push_back(key);
    }

    void undo_null(const Undo& u) {
        repKeys.pop_back();
        sideToMove ^= 1;
        castlingRights = u.castlingRights;
        epSquare = u.epSquare;
        rule50 = u.rule50;
        fullmove = u.fullmove;
        key = u.key;
    }

    bool is_repetition() const {
        if (repKeys.size() < 4) return false;
        int limit = std::max(0, int(repKeys.size()) - 1 - rule50);
        for (int i = int(repKeys.size()) - 3; i >= limit; i -= 2)
            if (repKeys[i] == key) return true;
        return false;
    }

    void gen_pawn_moves(MoveList& ml, int c, bool capturesOnly) const {
        U64 pawns = bb[c][PAWN];
        int push = (c == WHITE) ? 8 : -8;
        int startRank = (c == WHITE) ? 1 : 6;
        int promoRank = (c == WHITE) ? 6 : 1;
        int ep = epSquare;
        while (pawns) {
            int sq = pop_lsb(pawns);
            int r = rank_of(sq);
            int f = file_of(sq);
            if (!capturesOnly) {
                int to = sq + push;
                if (to >= 0 && to < 64 && board[to] == EMPTY) {
                    if (r == promoRank) {
                        ml.add(make_move(sq, to, 4));
                        ml.add(make_move(sq, to, 3));
                        ml.add(make_move(sq, to, 2));
                        ml.add(make_move(sq, to, 1));
                    } else {
                        ml.add(make_move(sq, to, 0, FLAG_NONE));
                        if (r == startRank) {
                            int to2 = sq + 2 * push;
                            if (board[to2] == EMPTY) ml.add(make_move(sq, to2, 0, FLAG_DOUBLE_PUSH));
                        }
                    }
                }
            }
            int capA = sq + ((c == WHITE) ? 7 : -9);
            int capB = sq + ((c == WHITE) ? 9 : -7);
            if (f > 0 && capA >= 0 && capA < 64) {
                if (board[capA] != EMPTY && piece_color(board[capA]) != c) {
                    if (r == promoRank) {
                        ml.add(make_move(sq, capA, 4, FLAG_CAPTURE));
                        ml.add(make_move(sq, capA, 3, FLAG_CAPTURE));
                        ml.add(make_move(sq, capA, 2, FLAG_CAPTURE));
                        ml.add(make_move(sq, capA, 1, FLAG_CAPTURE));
                    } else {
                        ml.add(make_move(sq, capA, 0, FLAG_CAPTURE));
                    }
                } else if (capA == ep) {
                    ml.add(make_move(sq, capA, 0, FLAG_CAPTURE | FLAG_EN_PASSANT));
                }
            }
            if (f < 7 && capB >= 0 && capB < 64) {
                if (board[capB] != EMPTY && piece_color(board[capB]) != c) {
                    if (r == promoRank) {
                        ml.add(make_move(sq, capB, 4, FLAG_CAPTURE));
                        ml.add(make_move(sq, capB, 3, FLAG_CAPTURE));
                        ml.add(make_move(sq, capB, 2, FLAG_CAPTURE));
                        ml.add(make_move(sq, capB, 1, FLAG_CAPTURE));
                    } else {
                        ml.add(make_move(sq, capB, 0, FLAG_CAPTURE));
                    }
                } else if (capB == ep) {
                    ml.add(make_move(sq, capB, 0, FLAG_CAPTURE | FLAG_EN_PASSANT));
                }
            }
        }
    }

    void gen_piece_moves(MoveList& ml, int c, int pt, bool capturesOnly) const {
        U64 pieces = bb[c][pt];
        U64 own = occ[c];
        U64 enemy = occ[c ^ 1];
        while (pieces) {
            int sq = pop_lsb(pieces);
            U64 att = 0;
            if (pt == KNIGHT) att = g_knightAtt[sq];
            else if (pt == BISHOP) att = bishop_attacks(sq, occAll);
            else if (pt == ROOK) att = rook_attacks(sq, occAll);
            else if (pt == QUEEN) att = bishop_attacks(sq, occAll) | rook_attacks(sq, occAll);
            else if (pt == KING) att = g_kingAtt[sq];
            att &= ~own;
            if (capturesOnly) att &= enemy;
            while (att) {
                int to = pop_lsb(att);
                int flags = (board[to] != EMPTY) ? FLAG_CAPTURE : FLAG_NONE;
                ml.add(make_move(sq, to, 0, flags));
            }
        }
    }

    void gen_castling(MoveList& ml, int c) const {
        if (in_check(c)) return;
        if (c == WHITE) {
            if ((castlingRights & CASTLE_WHITE_OO) && board[5] == EMPTY && board[6] == EMPTY
                && !square_attacked(5, BLACK) && !square_attacked(6, BLACK))
                ml.add(make_move(4, 6, 0, FLAG_CASTLE));
            if ((castlingRights & CASTLE_WHITE_OOO) && board[3] == EMPTY && board[2] == EMPTY && board[1] == EMPTY
                && !square_attacked(3, BLACK) && !square_attacked(2, BLACK))
                ml.add(make_move(4, 2, 0, FLAG_CASTLE));
        } else {
            if ((castlingRights & CASTLE_BLACK_OO) && board[61] == EMPTY && board[62] == EMPTY
                && !square_attacked(61, WHITE) && !square_attacked(62, WHITE))
                ml.add(make_move(60, 62, 0, FLAG_CASTLE));
            if ((castlingRights & CASTLE_BLACK_OOO) && board[59] == EMPTY && board[58] == EMPTY && board[57] == EMPTY
                && !square_attacked(59, WHITE) && !square_attacked(58, WHITE))
                ml.add(make_move(60, 58, 0, FLAG_CASTLE));
        }
    }

    void generate_pseudo(MoveList& ml, bool capturesOnly) const {
        int us = sideToMove;
        gen_pawn_moves(ml, us, capturesOnly);
        gen_piece_moves(ml, us, KNIGHT, capturesOnly);
        gen_piece_moves(ml, us, BISHOP, capturesOnly);
        gen_piece_moves(ml, us, ROOK, capturesOnly);
        gen_piece_moves(ml, us, QUEEN, capturesOnly);
        gen_piece_moves(ml, us, KING, capturesOnly);
        if (!capturesOnly) gen_castling(ml, us);
    }

    void generate_legal(MoveList& legal, bool capturesOnly) {
        MoveList pseudo;
        generate_pseudo(pseudo, capturesOnly);
        int us = sideToMove;
        for (int i = 0; i < pseudo.size; ++i) {
            Undo u;
            Move m = pseudo.moves[i];
            if (!do_move(m, u)) continue;
            bool ok = !in_check(us);
            undo_move(m, u);
            if (ok) legal.add(m);
        }
    }

    std::string fen() const {
        std::ostringstream os;
        for (int r = 7; r >= 0; --r) {
            int empty = 0;
            for (int f = 0; f < 8; ++f) {
                int sq = r * 8 + f;
                Piece p = board[sq];
                if (p == EMPTY) {
                    ++empty;
                    continue;
                }
                if (empty) {
                    os << empty;
                    empty = 0;
                }
                char c = '?';
                switch (p) {
                case W_PAWN: c = 'P'; break;
                case W_KNIGHT: c = 'N'; break;
                case W_BISHOP: c = 'B'; break;
                case W_ROOK: c = 'R'; break;
                case W_QUEEN: c = 'Q'; break;
                case W_KING: c = 'K'; break;
                case B_PAWN: c = 'p'; break;
                case B_KNIGHT: c = 'n'; break;
                case B_BISHOP: c = 'b'; break;
                case B_ROOK: c = 'r'; break;
                case B_QUEEN: c = 'q'; break;
                case B_KING: c = 'k'; break;
                default: break;
                }
                os << c;
            }
            if (empty) os << empty;
            if (r) os << '/';
        }
        os << ' ' << (sideToMove == WHITE ? 'w' : 'b') << ' ';
        std::string c;
        if (castlingRights & CASTLE_WHITE_OO) c += 'K';
        if (castlingRights & CASTLE_WHITE_OOO) c += 'Q';
        if (castlingRights & CASTLE_BLACK_OO) c += 'k';
        if (castlingRights & CASTLE_BLACK_OOO) c += 'q';
        os << (c.empty() ? "-" : c) << ' ';
        os << (epSquare == -1 ? "-" : square_to_string(epSquare));
        os << ' ' << rule50 << ' ' << fullmove;
        return os.str();
    }
};

struct SpinLock {
    std::atomic_flag flag = ATOMIC_FLAG_INIT;
    void lock() {
        while (flag.test_and_set(std::memory_order_acquire)) {}
    }
    void unlock() { flag.clear(std::memory_order_release); }
};

enum Bound : std::uint8_t { BOUND_NONE = 0, BOUND_ALPHA = 1, BOUND_BETA = 2, BOUND_EXACT = 3 };

struct TTEntry {
    U64 key = 0;
    std::int16_t score = 0;
    std::int16_t eval = 0;
    std::int8_t depth = 0;
    std::uint8_t age = 0;
    std::uint8_t bound = 0;
    std::uint16_t move = 0;
};

struct TranspositionTable {
    std::vector<TTEntry> table;
    std::array<SpinLock, 4096> locks{};
    std::size_t mask = 0;
    std::atomic<std::uint8_t> age{0};

    void resize_mb(std::size_t mb) {
        if (mb < 1) mb = 1;
        std::size_t bytes = mb * 1024ULL * 1024ULL;
        std::size_t n = 1;
        while (n * sizeof(TTEntry) < bytes) n <<= 1;
        if (n < 1) n = 1;
        table.assign(n, TTEntry{});
        mask = n - 1;
    }

    void clear() {
        std::fill(table.begin(), table.end(), TTEntry{});
    }

    int hashfull_permille() const {
        if (table.empty()) return 0;
        std::size_t sample = std::min<std::size_t>(1000, table.size());
        std::uint8_t a = age.load(std::memory_order_relaxed);
        using Diff = std::vector<TTEntry>::difference_type;
        std::size_t used = std::count_if(table.begin(), table.begin() + static_cast<Diff>(sample), [a](const TTEntry& e) {
            return e.key && e.age == a;
        });
        return int((used * 1000ULL) / sample);
    }

    bool probe(U64 key, TTEntry& out) const {
        if (table.empty()) return false;
        const TTEntry& e = table[key & mask];
        if (e.key == key) {
            out = e;
            return true;
        }
        return false;
    }

    void store(U64 key, int depth, int score, int eval, int bound, Move move, int ply) {
        if (table.empty()) return;
        std::size_t idx = key & mask;
        SpinLock& lk = locks[idx & (locks.size() - 1)];
        lk.lock();
        TTEntry& e = table[idx];
        int s = score;
        if (s > MATE_SCORE - MAX_PLY) s += ply;
        else if (s < -MATE_SCORE + MAX_PLY) s -= ply;

        bool replace = (e.key != key) || (bound == BOUND_EXACT) || (depth + 2 >= e.depth)
                    || (e.age != age.load(std::memory_order_relaxed));

        if (replace) {
            e.key = key;
            e.depth = std::int8_t(std::max(-127, std::min(127, depth)));
            e.score = std::int16_t(std::max(-32000, std::min(32000, s)));
            e.eval = std::int16_t(std::max(-32000, std::min(32000, eval)));
            e.bound = std::uint8_t(bound);
            e.move = std::uint16_t(move & 0xFFFF);
            e.age = age.load(std::memory_order_relaxed);
        }
        lk.unlock();
    }
};

struct Limits {
    int wtime = -1, btime = -1;
    int winc = 0, binc = 0;
    int movestogo = 0;
    int depth = 0;
    std::uint64_t nodes = 0;
    int movetime = 0;
    bool infinite = false;
};

struct TimeBudget {
    std::int64_t softMs = 0;
    std::int64_t hardMs = 0;
};

struct RootInfo {
    Move bestMove = 0;
    Move ponderMove = 0;
    int bestScore = -INF;
    int depth = 0;
    int seldepth = 0;
    std::vector<Move> pv;
    std::uint64_t nodes = 0;
};

struct SearchParams {
    int threads = 1;
    int hashMB = 16;
    bool ponder = false;
    int multiPV = 1;
    bool chess960 = false;
    int moveOverhead = 200;
    std::string syzygyPath;
    int syzygyProbeDepth = 1;
    int syzygyProbeLimit = 7;
};

inline int relative_sq(int c, int sq) { return (c == WHITE) ? sq : (sq ^ 56); }

static const int kPhaseInc[6] = {0, 1, 1, 2, 4, 0};
static const int kPieceValueMg[6] = {100, 325, 340, 500, 975, 0};
static const int kPieceValueEg[6] = {120, 305, 330, 520, 940, 0};

static const int kPstPawnMg[64] = {
    0, 0, 0, 0, 0, 0, 0, 0, 42, 48, 32, 40, 40, 34, 46, 40, 8, 10, 18, 26, 30, 20, 12, 6,
    -8, -2, 4, 24, 28, 14, -2, -8, -12, -8, 2, 20, 24, 8, -8, -14, -10, -6, -2, 6, 10, 0, -8, -12,
    -6, -2, 2, -10, -8, 2, -4, -8, 0, 0, 0, 0, 0, 0, 0, 0
};
static const int kPstPawnEg[64] = {
    0, 0, 0, 0, 0, 0, 0, 0, 40, 34, 30, 22, 22, 26, 30, 34, 20, 16, 12, 8, 8, 12, 16, 20,
    10, 8, 6, 0, 0, 6, 8, 10, 4, 2, 0, -6, -6, 0, 2, 4, -2, -4, -6, -8, -8, -6, -4, -2,
    -8, -8, -8, -8, -8, -8, -8, -8, 0, 0, 0, 0, 0, 0, 0, 0
};
static const int kPstKnightMg[64] = {
    -55, -34, -20, -14, -14, -20, -34, -55, -28, -8, 8, 14, 14, 8, -8, -28, -14, 10, 22, 28, 28, 22, 10, -14,
    -8, 16, 28, 36, 36, 28, 16, -8, -8, 16, 28, 34, 34, 28, 16, -8, -14, 6, 18, 24, 24, 18, 6, -14,
    -24, -8, 2, 10, 10, 2, -8, -24, -45, -24, -12, -8, -8, -12, -24, -45
};
static const int kPstKnightEg[64] = {
    -36, -22, -10, -6, -6, -10, -22, -36, -18, -6, 2, 8, 8, 2, -6, -18, -8, 4, 12, 18, 18, 12, 4, -8,
    -4, 10, 18, 24, 24, 18, 10, -4, -4, 10, 18, 22, 22, 18, 10, -4, -8, 4, 12, 16, 16, 12, 4, -8,
    -16, -6, 2, 8, 8, 2, -6, -16, -30, -18, -8, -4, -4, -8, -18, -30
};
static const int kPstBishopMg[64] = {
    -20, -10, -10, -10, -10, -10, -10, -20, -10, 4, 2, 4, 4, 2, 4, -10, -8, 2, 10, 12, 12, 10, 2, -8,
    -6, 8, 12, 18, 18, 12, 8, -6, -6, 8, 12, 16, 16, 12, 8, -6, -8, 2, 8, 12, 12, 8, 2, -8,
    -10, 0, 2, 4, 4, 2, 0, -10, -20, -10, -10, -10, -10, -10, -10, -20
};
static const int kPstBishopEg[64] = {
    -16, -10, -8, -8, -8, -8, -10, -16, -10, 0, 2, 4, 4, 2, 0, -10, -8, 4, 8, 10, 10, 8, 4, -8,
    -6, 8, 10, 14, 14, 10, 8, -6, -6, 8, 10, 14, 14, 10, 8, -6, -8, 4, 8, 10, 10, 8, 4, -8,
    -10, 0, 2, 4, 4, 2, 0, -10, -16, -10, -8, -8, -8, -8, -10, -16
};
static const int kPstRookMg[64] = {
    0, 0, 4, 8, 8, 4, 0, 0, -2, 0, 2, 6, 6, 2, 0, -2, -4, 0, 0, 4, 4, 0, 0, -4,
    -6, -2, 0, 2, 2, 0, -2, -6, -6, -2, 0, 2, 2, 0, -2, -6, -4, 0, 0, 4, 4, 0, 0, -4,
    2, 8, 10, 12, 12, 10, 8, 2, 0, 0, 4, 8, 8, 4, 0, 0
};
static const int kPstRookEg[64] = {
    6, 8, 8, 10, 10, 8, 8, 6, 6, 8, 8, 10, 10, 8, 8, 6, 4, 6, 8, 10, 10, 8, 6, 4,
    2, 4, 6, 8, 8, 6, 4, 2, 0, 2, 4, 6, 6, 4, 2, 0, -2, 0, 2, 4, 4, 2, 0, -2,
    -2, 0, 2, 4, 4, 2, 0, -2, 0, 2, 4, 6, 6, 4, 2, 0
};
static const int kPstQueenMg[64] = {
    -16, -10, -8, -4, -4, -8, -10, -16, -10, -4, -2, 0, 0, -2, -4, -10, -8, -2, 2, 4, 4, 2, -2, -8,
    -4, 0, 4, 6, 6, 4, 0, -4, -4, 0, 4, 6, 6, 4, 0, -4, -8, -2, 2, 4, 4, 2, -2, -8,
    -12, -6, -2, 0, 0, -2, -6, -12, -18, -12, -8, -6, -6, -8, -12, -18
};
static const int kPstQueenEg[64] = {
    -10, -8, -6, -4, -4, -6, -8, -10, -8, -4, -2, 0, 0, -2, -4, -8, -6, -2, 2, 4, 4, 2, -2, -6,
    -4, 0, 4, 6, 6, 4, 0, -4, -4, 0, 4, 6, 6, 4, 0, -4, -6, -2, 2, 4, 4, 2, -2, -6,
    -8, -4, -2, 0, 0, -2, -4, -8, -10, -8, -6, -4, -4, -6, -8, -10
};
static const int kPstKingMg[64] = {
    24, 34, 16, 0, 0, 16, 34, 24, 16, 24, 8, -6, -6, 8, 24, 16, 4, 8, -4, -16, -16, -4, 8, 4,
    -8, -6, -14, -22, -22, -14, -6, -8, -18, -16, -22, -28, -28, -22, -16, -18, -24, -24, -26, -30, -30, -26, -24, -24,
    -20, -12, -10, -14, -14, -10, -12, -20, 0, 8, 6, -6, -6, 6, 8, 0
};
static const int kPstKingEg[64] = {
    -18, -12, -8, -6, -6, -8, -12, -18, -12, -6, -2, 0, 0, -2, -6, -12, -8, -2, 4, 8, 8, 4, -2, -8,
    -6, 0, 8, 12, 12, 8, 0, -6, -6, 0, 8, 12, 12, 8, 0, -6, -8, -2, 4, 8, 8, 4, -2, -8,
    -12, -6, -2, 0, 0, -2, -6, -12, -18, -12, -8, -6, -6, -8, -12, -18
};

int piece_square_mg(int pt, int sq) {
    switch (pt) {
    case PAWN: return kPstPawnMg[sq];
    case KNIGHT: return kPstKnightMg[sq];
    case BISHOP: return kPstBishopMg[sq];
    case ROOK: return kPstRookMg[sq];
    case QUEEN: return kPstQueenMg[sq];
    case KING: return kPstKingMg[sq];
    default: return 0;
    }
}
int piece_square_eg(int pt, int sq) {
    switch (pt) {
    case PAWN: return kPstPawnEg[sq];
    case KNIGHT: return kPstKnightEg[sq];
    case BISHOP: return kPstBishopEg[sq];
    case ROOK: return kPstRookEg[sq];
    case QUEEN: return kPstQueenEg[sq];
    case KING: return kPstKingEg[sq];
    default: return 0;
    }
}

int opening_development_eval(const Position& pos) {
    int score = 0;
    int phase = 0;
    for (int c = 0; c < 2; ++c) {
        for (int pt = KNIGHT; pt <= QUEEN; ++pt) {
            phase += kPhaseInc[pt] * popcount(pos.bb[c][pt]);
        }
    }
    int openingWeight = std::max(0, std::min(16, phase - 8));
    if (openingWeight == 0) return 0;

    auto add_side = [&](int c) {
        int sign = (c == WHITE) ? 1 : -1;
        int s = 0;
        U64 pawns = pos.bb[c][PAWN];
        U64 pawnCenterAtt = 0;
        while (pawns) {
            int sq = pop_lsb(pawns);
            int f = file_of(sq);
            int r = rank_of(sq);

            pawnCenterAtt |= g_pawnAtt[c][sq];

            if (f >= 3 && f <= 4) {
                int adv = (c == WHITE) ? (r - 1) : (6 - r);
                if (adv > 0) s += 10 + adv * 4;
            }

            // Discourage early flank pawn pushes in the opening.
            if (f <= 1 || f >= 6) {
                int adv = (c == WHITE) ? (r - 1) : (6 - r);
                if (adv > 0) s -= 12 + adv * 6;
            }
        }

        static const U64 kCenter4 = (bit(27) | bit(28) | bit(35) | bit(36));  // d4 e4 d5 e5
        s += 5 * popcount(pawnCenterAtt & kCenter4);

        int kHomeA = (c == WHITE) ? 1 : 57;
        int kHomeB = (c == WHITE) ? 6 : 62;
        int bHomeA = (c == WHITE) ? 2 : 58;
        int bHomeB = (c == WHITE) ? 5 : 61;
        if (pos.board[kHomeA] == make_piece(c, KNIGHT)) s -= 11;
        if (pos.board[kHomeB] == make_piece(c, KNIGHT)) s -= 11;
        if (pos.board[bHomeA] == make_piece(c, BISHOP)) s -= 9;
        if (pos.board[bHomeB] == make_piece(c, BISHOP)) s -= 9;

        int ksq = pos.kingSq[c];
        if (ksq == ((c == WHITE) ? 6 : 62) || ksq == ((c == WHITE) ? 2 : 58)) s += 18;
        if (ksq == ((c == WHITE) ? 4 : 60)) {
            int rights = (c == WHITE) ? (CASTLE_WHITE_OO | CASTLE_WHITE_OOO) : (CASTLE_BLACK_OO | CASTLE_BLACK_OOO);
            if ((pos.castlingRights & rights) == 0) s -= 20;
        }

        score += sign * s;
    };

    add_side(WHITE);
    add_side(BLACK);
    return (score * openingWeight) / 16;
}

bool insufficient_material(const Position& pos) {
    if (pos.bb[WHITE][PAWN] || pos.bb[BLACK][PAWN] || pos.bb[WHITE][ROOK] || pos.bb[BLACK][ROOK]
        || pos.bb[WHITE][QUEEN] || pos.bb[BLACK][QUEEN])
        return false;
    int wb = popcount(pos.bb[WHITE][BISHOP]);
    int wn = popcount(pos.bb[WHITE][KNIGHT]);
    int bb = popcount(pos.bb[BLACK][BISHOP]);
    int bn = popcount(pos.bb[BLACK][KNIGHT]);
    if (wb + wn <= 1 && bb + bn <= 1) return true;
    if (wn == 0 && bn == 0 && wb == 1 && bb == 1) return true;
    return false;
}

int eval_side_king_safety(const Position& pos, int c) {
    int ksq = pos.kingSq[c];
    U64 zone = g_kingAtt[ksq] | bit(ksq);
    int enemy = c ^ 1;
    int attacks = 0;
    int attackers = 0;

    U64 p = pos.bb[enemy][PAWN];
    while (p) {
        int sq = pop_lsb(p);
        U64 hit = g_pawnAtt[enemy][sq] & zone;
        if (hit) {
            attacks += popcount(hit);
            ++attackers;
        }
    }
    U64 n = pos.bb[enemy][KNIGHT];
    while (n) {
        int sq = pop_lsb(n);
        U64 hit = g_knightAtt[sq] & zone;
        if (hit) {
            attacks += popcount(hit);
            ++attackers;
        }
    }
    U64 b = pos.bb[enemy][BISHOP];
    while (b) {
        int sq = pop_lsb(b);
        U64 hit = bishop_attacks(sq, pos.occAll) & zone;
        if (hit) {
            attacks += popcount(hit);
            ++attackers;
        }
    }
    U64 r = pos.bb[enemy][ROOK];
    while (r) {
        int sq = pop_lsb(r);
        U64 hit = rook_attacks(sq, pos.occAll) & zone;
        if (hit) {
            attacks += popcount(hit);
            ++attackers;
        }
    }
    U64 q = pos.bb[enemy][QUEEN];
    while (q) {
        int sq = pop_lsb(q);
        U64 hit = (rook_attacks(sq, pos.occAll) | bishop_attacks(sq, pos.occAll)) & zone;
        if (hit) {
            attacks += popcount(hit);
            ++attackers;
        }
    }

    int shield = 0;
    int rf = rank_of(ksq);
    int ff = file_of(ksq);
    int dir = (c == WHITE) ? 1 : -1;
    int tr = rf + dir;
    if (tr >= 0 && tr < 8) {
        for (int df = -1; df <= 1; ++df) {
            int tf = ff + df;
            if (tf < 0 || tf >= 8) continue;
            int sq = tr * 8 + tf;
            Piece pce = pos.board[sq];
            if (pce != EMPTY && piece_color(pce) == c && piece_type(pce) == PAWN) shield += 6;
            else shield -= 4;
        }
    }
    int openFiles = 0;
    for (int df = -1; df <= 1; ++df) {
        int f = ff + df;
        if (f < 0 || f >= 8) continue;
        if ((pos.bb[c][PAWN] & g_fileMask[f]) == 0) ++openFiles;
    }

    int pressure = attacks * 10 + attackers * 10 + openFiles * 7 - shield;
    if (attackers >= 2) pressure += attacks * 2;
    return pressure;
}

int evaluate(const Position& pos) {
    if (insufficient_material(pos)) return 0;

    int mg = 0, eg = 0, phase = 0;
    const int bishopCount[2] = {popcount(pos.bb[WHITE][BISHOP]), popcount(pos.bb[BLACK][BISHOP])};
    int pawnFiles[2][8] = {};
    U64 attackMap[2] = {0ULL, 0ULL};
    U64 pawnAttackMap[2] = {0ULL, 0ULL};
    U64 heavyAttackMap[2] = {0ULL, 0ULL};

    for (int c = 0; c < 2; ++c) {
        int sign = (c == WHITE) ? 1 : -1;
        for (int pt = PAWN; pt <= KING; ++pt) {
            U64 bits = pos.bb[c][pt];
            phase += kPhaseInc[pt] * popcount(bits);
            while (bits) {
                int sq = pop_lsb(bits);
                int rsq = relative_sq(c, sq);
                mg += sign * (kPieceValueMg[pt] + piece_square_mg(pt, rsq));
                eg += sign * (kPieceValueEg[pt] + piece_square_eg(pt, rsq));
                if (pt == PAWN) pawnFiles[c][file_of(sq)]++;
            }
        }
    }

    if (bishopCount[WHITE] >= 2) {
        mg += 32;
        eg += 40;
    }
    if (bishopCount[BLACK] >= 2) {
        mg -= 32;
        eg -= 40;
    }

    for (int c = 0; c < 2; ++c) {
        int sign = (c == WHITE) ? 1 : -1;
        U64 pawns = pos.bb[c][PAWN];
        U64 enemyPawns = pos.bb[c ^ 1][PAWN];
        while (pawns) {
            int sq = pop_lsb(pawns);
            int f = file_of(sq);
            int r = rank_of(sq);

            bool isolated = (pos.bb[c][PAWN] & g_adjacentFileMask[f]) == 0;
            if (isolated) {
                mg -= sign * 12;
                eg -= sign * 10;
            }

            if (pawnFiles[c][f] > 1) {
                mg -= sign * 10;
                eg -= sign * 12;
            }

            bool passed = (enemyPawns & g_passedMask[c][sq]) == 0;
            if (passed) {
                int bonus = (c == WHITE) ? r : (7 - r);
                mg += sign * (14 + bonus * 8);
                eg += sign * (22 + bonus * 12);
            }
        }
    }

    for (int c = 0; c < 2; ++c) {
        int sign = (c == WHITE) ? 1 : -1;
        U64 own = pos.occ[c];
        U64 n = pos.bb[c][KNIGHT];
        while (n) {
            int sq = pop_lsb(n);
            U64 att = g_knightAtt[sq];
            int mob = popcount(att & ~own);
            mg += sign * (mob * 4);
            eg += sign * (mob * 3);
            attackMap[c] |= att;
        }
        U64 b = pos.bb[c][BISHOP];
        while (b) {
            int sq = pop_lsb(b);
            U64 att = bishop_attacks(sq, pos.occAll);
            int mob = popcount(att & ~own);
            mg += sign * (mob * 4);
            eg += sign * (mob * 4);
            attackMap[c] |= att;
        }
        U64 r = pos.bb[c][ROOK];
        while (r) {
            int sq = pop_lsb(r);
            U64 att = rook_attacks(sq, pos.occAll);
            int mob = popcount(att & ~own);
            mg += sign * (mob * 2);
            eg += sign * (mob * 3);
            attackMap[c] |= att;
            heavyAttackMap[c] |= att;
        }
        U64 q = pos.bb[c][QUEEN];
        while (q) {
            int sq = pop_lsb(q);
            U64 att = rook_attacks(sq, pos.occAll) | bishop_attacks(sq, pos.occAll);
            int mob = popcount(att & ~own);
            mg += sign * mob;
            eg += sign * (mob * 2);
            attackMap[c] |= att;
            heavyAttackMap[c] |= att;
        }
        attackMap[c] |= g_kingAtt[pos.kingSq[c]];

        U64 pawnAtk = 0;
        U64 pp = pos.bb[c][PAWN];
        while (pp) {
            int sq = pop_lsb(pp);
            pawnAtk |= g_pawnAtt[c][sq];
        }
        pawnAttackMap[c] = pawnAtk;
        attackMap[c] |= pawnAtk;
        int threats = popcount(pawnAtk & pos.occ[c ^ 1] & ~(pos.bb[c ^ 1][PAWN] | pos.bb[c ^ 1][KING]));
        mg += sign * (threats * 12);
        eg += sign * (threats * 8);
    }

    auto king_danger_layer = [&](int defender) -> int {
        int attacker = defender ^ 1;
        int ksq = pos.kingSq[defender];
        U64 zone = g_kingAtt[ksq] | bit(ksq);

        int zonePressure = popcount(attackMap[attacker] & zone);
        int pawnPressure = popcount(pawnAttackMap[attacker] & zone);
        int heavyPressure = popcount(heavyAttackMap[attacker] & zone);
        int attackers = 0;

        U64 n = pos.bb[attacker][KNIGHT];
        while (n) {
            int sq = pop_lsb(n);
            if (g_knightAtt[sq] & zone) ++attackers;
        }
        U64 b = pos.bb[attacker][BISHOP];
        while (b) {
            int sq = pop_lsb(b);
            if (bishop_attacks(sq, pos.occAll) & zone) ++attackers;
        }
        U64 r = pos.bb[attacker][ROOK];
        while (r) {
            int sq = pop_lsb(r);
            if (rook_attacks(sq, pos.occAll) & zone) ++attackers;
        }
        U64 q = pos.bb[attacker][QUEEN];
        while (q) {
            int sq = pop_lsb(q);
            if ((rook_attacks(sq, pos.occAll) | bishop_attacks(sq, pos.occAll)) & zone) ++attackers;
        }

        int ff = file_of(ksq);
        int rf = rank_of(ksq);
        int dir = (defender == WHITE) ? 1 : -1;
        int tr = rf + dir;
        int shelter = 0;
        if (tr >= 0 && tr < 8) {
            for (int df = -1; df <= 1; ++df) {
                int tf = ff + df;
                if (tf < 0 || tf >= 8) continue;
                int sq = tr * 8 + tf;
                Piece p = pos.board[sq];
                if (p != EMPTY && piece_color(p) == defender && piece_type(p) == PAWN) shelter += 7;
                else shelter -= 4;
            }
        }
        int openFiles = 0;
        for (int df = -1; df <= 1; ++df) {
            int f = ff + df;
            if (f < 0 || f >= 8) continue;
            if ((pos.bb[defender][PAWN] & g_fileMask[f]) == 0) ++openFiles;
        }

        int score = zonePressure * 6 + pawnPressure * 8 + heavyPressure * 9 + attackers * 12 + openFiles * 8 - shelter;
        if (attackers >= 2) score += zonePressure * 2;
        return std::max(0, score);
    };

    auto threat_layer = [&](int us) -> int {
        int them = us ^ 1;
        int sc = 0;
        U64 targets = pos.occ[them] & ~(pos.bb[them][KING]);
        while (targets) {
            int sq = pop_lsb(targets);
            Piece p = pos.board[sq];
            if (p == EMPTY) continue;
            int pt = piece_type(p);
            int val = kPieceValueMg[pt];
            U64 sqBit = bit(sq);
            bool attacked = (attackMap[us] & sqBit) != 0;
            if (!attacked) continue;
            bool defended = (attackMap[them] & sqBit) != 0;
            if (!defended) sc += 6 + val / 28;
            if ((pawnAttackMap[us] & sqBit) != 0) sc += 8 + val / 22;
            if (pt != PAWN && pt != KING) sc += 3 + val / 40;
        }

        U64 weakPawns = pos.bb[them][PAWN] & attackMap[us] & ~attackMap[them];
        sc += popcount(weakPawns) * 9;
        return sc;
    };

    int kdWhite = king_danger_layer(WHITE);
    int kdBlack = king_danger_layer(BLACK);
    mg += (kdBlack - kdWhite);
    eg += (kdBlack - kdWhite) / 4;

    int thWhite = threat_layer(WHITE);
    int thBlack = threat_layer(BLACK);
    mg += (thWhite - thBlack);
    eg += (thWhite - thBlack) / 2;
    mg += opening_development_eval(pos);

    phase = std::min(24, phase);
    int score = (mg * phase + eg * (24 - phase)) / 24;
    score += (pos.sideToMove == WHITE) ? 8 : -8;
    return (pos.sideToMove == WHITE) ? score : -score;
}

struct SearchThreadState {
    std::array<std::array<Move, MAX_PLY>, MAX_PLY> pv{};
    std::array<int, MAX_PLY> pvLen{};
    std::array<std::array<Move, 2>, MAX_PLY> killers{};
    int history[2][64][64]{};
    Move counter[64][64]{};
    int selDepth = 0;
};

struct SearchControl {
    std::atomic<bool> stop{false};
    std::atomic<bool> pondering{false};
    std::atomic<std::uint64_t> globalNodes{0};
    Clock::time_point startTime{};
    std::int64_t softMs = 0;
    std::int64_t hardMs = 0;
    Limits limits{};
};

struct Searcher {
    Position pos;
    TranspositionTable* tt = nullptr;
    SearchControl* ctrl = nullptr;
    SearchParams params{};
    SearchThreadState st{};
    int threadId = 0;
    bool outputInfo = false;
    std::uint64_t nodes = 0;
    std::mt19937_64 rng{0xDEADBEEF1234ULL};

    explicit Searcher(const Position& p, TranspositionTable* table, SearchControl* control,
                      const SearchParams& sp, int id, bool infoOut) :
        pos(p), tt(table), ctrl(control), params(sp), threadId(id), outputInfo(infoOut) {
        rng.seed(0x9E3779B97F4A7C15ULL ^ U64(id + 1) ^ pos.key);
    }

    bool should_stop() {
        if (ctrl->stop.load(std::memory_order_relaxed)) return true;
        if (ctrl->limits.nodes > 0) {
            if (ctrl->globalNodes.load(std::memory_order_relaxed) >= ctrl->limits.nodes) {
                ctrl->stop.store(true, std::memory_order_relaxed);
                return true;
            }
        }
        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - ctrl->startTime).count();
        if (!ctrl->limits.infinite && ctrl->hardMs > 0 && elapsed >= ctrl->hardMs) {
            ctrl->stop.store(true, std::memory_order_relaxed);
            return true;
        }
        return false;
    }

    static int score_from_tt(int sc, int ply) {
        int v = sc;
        if (v > MATE_SCORE - MAX_PLY) v -= ply;
        else if (v < -MATE_SCORE + MAX_PLY) v += ply;
        return v;
    }

    int capture_gain(Move m) const {
        Piece cap = (move_flags(m) & FLAG_EN_PASSANT) ? make_piece(pos.sideToMove ^ 1, PAWN) : pos.board[move_to(m)];
        if (cap == EMPTY) return 0;
        int gain = kPieceValueMg[piece_type(cap)];
        int promo = move_promo(m);
        if (promo) {
            int pt = QUEEN;
            if (promo == 1) pt = KNIGHT;
            else if (promo == 2) pt = BISHOP;
            else if (promo == 3) pt = ROOK;
            gain += kPieceValueMg[pt] - kPieceValueMg[PAWN];
        }
        return gain;
    }

    int move_score(Move m, Move ttMove, int ply, Move prevMove) const {
        if (m == ttMove) return 2000000000;
        if (pos.is_capture_move(m)) {
            Piece cap = (move_flags(m) & FLAG_EN_PASSANT) ? make_piece(pos.sideToMove ^ 1, PAWN) : pos.board[move_to(m)];
            Piece att = pos.board[move_from(m)];
            int cVal = (cap == EMPTY) ? 0 : kPieceValueMg[piece_type(cap)];
            int aVal = (att == EMPTY) ? 0 : kPieceValueMg[piece_type(att)];
            int promo = move_promo(m);
            return 1000000 + cVal * 16 - aVal + promo * 64;
        }
        int s = st.history[pos.sideToMove][move_from(m)][move_to(m)];
        if (st.killers[ply][0] == m) s += 500000;
        if (st.killers[ply][1] == m) s += 300000;
        if (prevMove) {
            Move c = st.counter[move_from(prevMove)][move_to(prevMove)];
            if (c == m) s += 120000;
        }
        return s;
    }

    void order_moves(std::vector<Move>& mv, Move ttMove, int ply, Move prevMove) const {
        std::vector<int> sc(mv.size());
        for (std::size_t i = 0; i < mv.size(); ++i) sc[i] = move_score(mv[i], ttMove, ply, prevMove);
        std::vector<std::size_t> ord(mv.size());
        for (std::size_t i = 0; i < ord.size(); ++i) ord[i] = i;
        std::stable_sort(ord.begin(), ord.end(), [&](std::size_t a, std::size_t b) { return sc[a] > sc[b]; });
        std::vector<Move> sorted;
        sorted.reserve(mv.size());
        for (std::size_t i : ord) sorted.push_back(mv[i]);
        mv.swap(sorted);
    }

    void score_movelist(const MoveList& ml, std::array<int, MAX_MOVES>& sc, Move ttMove, int ply, Move prevMove) const {
        for (int i = 0; i < ml.size; ++i) sc[i] = move_score(ml.moves[i], ttMove, ply, prevMove);
    }

    void update_pv(int ply, Move m) {
        st.pv[ply][0] = m;
        int n = st.pvLen[ply + 1];
        for (int i = 0; i < n; ++i) st.pv[ply][i + 1] = st.pv[ply + 1][i];
        st.pvLen[ply] = n + 1;
    }

    int qsearch(int ply, int alpha, int beta) {
        if (ply >= MAX_PLY - 1) return evaluate(pos);
        if ((++nodes & 2047ULL) == 0) {
            ctrl->globalNodes.fetch_add(2048, std::memory_order_relaxed);
            if (should_stop()) return 0;
        }

        bool inCheck = pos.in_check(pos.sideToMove);
        int stand = -INF;
        if (!inCheck) {
            stand = evaluate(pos);
            if (stand >= beta) return stand;
            if (stand > alpha) alpha = stand;
        }

        MoveList ml;
        pos.generate_legal(ml, !inCheck);
        if (ml.size == 0) {
            if (inCheck) return -MATE_SCORE + ply;
            return 0;
        }

        std::array<int, MAX_MOVES> sc{};
        score_movelist(ml, sc, 0, ply, 0);
        for (int mi = 0; mi < ml.size; ++mi) {
            int best = mi;
            for (int j = mi + 1; j < ml.size; ++j) {
                if (sc[j] > sc[best]) best = j;
            }
            if (best != mi) {
                std::swap(sc[mi], sc[best]);
                std::swap(ml.moves[mi], ml.moves[best]);
            }
            Move m = ml.moves[mi];
            if (!inCheck) {
                int gain = capture_gain(m);
                if (move_promo(m) == 0) {
                    // Delta pruning + basic bad-capture filter in qsearch.
                    if (stand + gain + 96 < alpha) continue;
                    Piece att = pos.board[move_from(m)];
                    if (att != EMPTY && gain + 32 < kPieceValueMg[piece_type(att)]) continue;
                }
            }
            Undo u;
            pos.do_move(m, u);
            int score = -qsearch(ply + 1, -beta, -alpha);
            pos.undo_move(m, u);
            if (ctrl->stop.load(std::memory_order_relaxed)) return 0;
            if (score >= beta) return score;
            if (score > alpha) alpha = score;
        }
        return alpha;
    }

    int search(int depth, int ply, int alpha, int beta, bool pvNode, Move prevMove, bool cutNode) {
        if (ply >= MAX_PLY - 1) return evaluate(pos);
        if ((++nodes & 2047ULL) == 0) {
            ctrl->globalNodes.fetch_add(2048, std::memory_order_relaxed);
            if (should_stop()) return 0;
        }
        st.selDepth = std::max(st.selDepth, ply);
        st.pvLen[ply] = 0;

        bool inCheck = pos.in_check(pos.sideToMove);
        if (pos.rule50 >= 100 || pos.is_repetition()) return 0;
        if (insufficient_material(pos)) return 0;

        if (!pvNode) {
            alpha = std::max(alpha, -MATE_SCORE + ply);
            beta = std::min(beta, MATE_SCORE - ply - 1);
            if (alpha >= beta) return alpha;
        }

        if (depth <= 0) return qsearch(ply, alpha, beta);

        int alphaOrig = alpha;
        TTEntry tte{};
        bool ttHit = tt->probe(pos.key, tte);
        Move ttMove = ttHit ? Move(tte.move) : 0;
        int ttScore = 0;
        int staticEval = 0;
        if (ttHit) {
            ttScore = score_from_tt(tte.score, ply);
            staticEval = tte.eval;
            if (!pvNode && tte.depth >= depth) {
                if (tte.bound == BOUND_EXACT) return ttScore;
                if (tte.bound == BOUND_ALPHA && ttScore <= alpha) return ttScore;
                if (tte.bound == BOUND_BETA && ttScore >= beta) return ttScore;
            }
        }

        if (inCheck) staticEval = -INF;
        else if (!ttHit) staticEval = evaluate(pos);

        if (!pvNode && !inCheck && depth <= 2) {
            int margin = 95 * depth;
            if (staticEval - margin >= beta) return staticEval - margin;
            if (depth == 1 && staticEval + margin <= alpha) return qsearch(ply, alpha, beta);
        }

        if (!pvNode && !inCheck && depth >= 3 && staticEval >= beta - 14 * depth && pos.has_non_pawn_material(pos.sideToMove)) {
            Undo u;
            pos.do_null(u);
            int r = 2 + depth / 4;
            int score = -search(depth - 1 - r, ply + 1, -beta, -beta + 1, false, 0, true);
            pos.undo_null(u);
            if (ctrl->stop.load(std::memory_order_relaxed)) return 0;
            if (score >= beta) return score;
        }

        MoveList ml;
        pos.generate_legal(ml, false);
        if (ml.size == 0) {
            if (inCheck) return -MATE_SCORE + ply;
            return 0;
        }

        std::array<int, MAX_MOVES> sc{};
        score_movelist(ml, sc, ttMove, ply, prevMove);

        int bestScore = -INF;
        Move bestMove = 0;
        int moveCount = 0;
        std::array<Move, MAX_MOVES> quietTried{};
        int quietCount = 0;

        for (int mi = 0; mi < ml.size; ++mi) {
            int best = mi;
            for (int j = mi + 1; j < ml.size; ++j) {
                if (sc[j] > sc[best]) best = j;
            }
            if (best != mi) {
                std::swap(sc[mi], sc[best]);
                std::swap(ml.moves[mi], ml.moves[best]);
            }

            Move m = ml.moves[mi];
            bool isCap = pos.is_capture_move(m);
            int ext = 0;
            if (inCheck) ext = 1;
            int newDepth = depth - 1 + ext;
            if (!pvNode && !inCheck && !isCap && depth <= 3 && moveCount > 4 && staticEval + 120 * depth <= alpha) {
                ++moveCount;
                continue;
            }

            Undo u;
            pos.do_move(m, u);
            bool givesCheck = pos.in_check(pos.sideToMove);
            int checkExt = 0;
            if (givesCheck && depth >= 4 && newDepth >= 2) {
                int victimDanger = eval_side_king_safety(pos, pos.sideToMove);
                if (victimDanger >= 58) checkExt = 1;
            }
            int childDepth = newDepth + checkExt;
            int score;
            if (moveCount == 0) {
                score = -search(childDepth, ply + 1, -beta, -alpha, pvNode, m, false);
            } else {
                int reduction = 0;
                if (depth >= 3 && moveCount >= 3 && !pvNode && !isCap && !inCheck && !givesCheck) {
                    reduction = 1 + (depth >= 6) + (moveCount >= 8);
                    if (cutNode) reduction++;
                    int hist = st.history[pos.sideToMove ^ 1][move_from(m)][move_to(m)];
                    if (hist > 5000) reduction = std::max(0, reduction - 1);
                    reduction = std::min(reduction, newDepth - 1);
                }
                score = -search(childDepth - reduction, ply + 1, -alpha - 1, -alpha, false, m, true);
                if (score > alpha && reduction > 0) {
                    score = -search(childDepth, ply + 1, -alpha - 1, -alpha, false, m, true);
                }
                if (score > alpha && score < beta) {
                    score = -search(childDepth, ply + 1, -beta, -alpha, true, m, false);
                }
            }
            pos.undo_move(m, u);
            if (ctrl->stop.load(std::memory_order_relaxed)) return 0;

            ++moveCount;
            if (!isCap && quietCount < MAX_MOVES) quietTried[quietCount++] = m;

            if (score > bestScore) {
                bestScore = score;
                bestMove = m;
            }
            if (score > alpha) {
                alpha = score;
                update_pv(ply, m);
            }
            if (alpha >= beta) {
                if (!isCap) {
                    st.killers[ply][1] = st.killers[ply][0];
                    st.killers[ply][0] = m;
                    int bonus = depth * depth + 6;
                    st.history[pos.sideToMove][move_from(m)][move_to(m)] += bonus;
                    if (prevMove) st.counter[move_from(prevMove)][move_to(prevMove)] = m;
                    for (int qi = 0; qi < quietCount; ++qi) {
                        Move q = quietTried[qi];
                        if (q == m) continue;
                        st.history[pos.sideToMove][move_from(q)][move_to(q)] -= bonus / 2;
                    }
                }
                break;
            }
        }

        if (bestScore == -INF) bestScore = alphaOrig;
        int bound = BOUND_EXACT;
        if (bestScore <= alphaOrig) bound = BOUND_ALPHA;
        else if (bestScore >= beta) bound = BOUND_BETA;
        tt->store(pos.key, depth, bestScore, staticEval, bound, bestMove, ply);
        return bestScore;
    }

    static std::string score_to_uci(int score) {
        if (score > MATE_SCORE - MAX_PLY) {
            int mate = (MATE_SCORE - score + 1) / 2;
            return "mate " + std::to_string(mate);
        }
        if (score < -MATE_SCORE + MAX_PLY) {
            int mate = (-MATE_SCORE - score) / 2;
            return "mate " + std::to_string(mate);
        }
        return "cp " + std::to_string(score);
    }

    int root_opening_bias(Move m) const {
        if (!m) return 0;
        int from = move_from(m);
        int to = move_to(m);
        Piece p = pos.board[from];
        if (p == EMPTY) return 0;

        int us = pos.sideToMove;
        int pt = piece_type(p);
        int f = file_of(to);
        int r = rank_of(to);
        int bias = 0;

        if (move_flags(m) & FLAG_CASTLE) bias += 120;

        if (pt == PAWN) {
            int adv = (us == WHITE) ? (r - 1) : (6 - r);
            if (f == 3 || f == 4) bias += 95 + adv * 16;          // d/e pawns
            else if (f == 2 || f == 5) bias += 30 + adv * 8;      // c/f pawns
            else bias -= 120 + adv * 24;                           // a/b/g/h pawns
            if (to == 27 || to == 28 || to == 35 || to == 36) bias += 25;  // center occupancy
        } else if (pt == KNIGHT) {
            if (us == WHITE && (to == 18 || to == 21)) bias += 110;   // Nc3/Nf3
            if (us == BLACK && (to == 42 || to == 45)) bias += 110;   // ...Nc6/...Nf6
            if (file_of(to) == 0 || file_of(to) == 7) bias -= 65;     // rim knights
        } else if (pt == BISHOP) {
            if (us == WHITE && (to == 26 || to == 29 || to == 18 || to == 21)) bias += 45;
            if (us == BLACK && (to == 34 || to == 37 || to == 42 || to == 45)) bias += 45;
        } else if (pt == QUEEN) {
            bias -= 30; // discourage early queen excursions
        } else if (pt == KING && !(move_flags(m) & FLAG_CASTLE)) {
            bias -= 180;
        }

        return bias;
    }

    std::vector<Move> legalize_pv_from_root(Move first, const std::array<Move, MAX_PLY>& tail, int tailLen) const {
        std::vector<Move> pv;
        if (!first) return pv;

        Position tmp = pos;
        auto try_push = [&](Move m) -> bool {
            if (!m) return false;
            MoveList ml;
            tmp.generate_legal(ml, false);
            for (int i = 0; i < ml.size; ++i) {
                if (ml.moves[i] == m) {
                    Undo u;
                    tmp.do_move(m, u);
                    pv.push_back(m);
                    return true;
                }
            }
            return false;
        };

        if (!try_push(first)) return {};
        for (int i = 0; i < tailLen; ++i) {
            if (!try_push(tail[i])) break;
        }
        return pv;
    }

    RootInfo iterative_deepening() {
        RootInfo ri{};
        MoveList ml;
        pos.generate_legal(ml, false);
        if (ml.size == 0) {
            ri.bestMove = 0;
            ri.bestScore = pos.in_check(pos.sideToMove) ? -MATE_SCORE : 0;
            return ri;
        }
        std::vector<Move> rootMoves;
        rootMoves.reserve(ml.size);
        for (int i = 0; i < ml.size; ++i) rootMoves.push_back(ml.moves[i]);
        if (threadId > 0 && rootMoves.size() > 1) {
            std::size_t rot = std::size_t(threadId) % rootMoves.size();
            using Diff = std::vector<Move>::difference_type;
            std::rotate(rootMoves.begin(), rootMoves.begin() + static_cast<Diff>(rot), rootMoves.end());
        }

        int maxDepth = (ctrl->limits.depth > 0) ? ctrl->limits.depth : 128;
        int alpha = -INF;
        int beta = INF;
        int prevScore = 0;
        auto now_ms = [&]() -> std::int64_t {
            return std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - ctrl->startTime).count();
        };

        for (int depth = 1; depth <= maxDepth; ++depth) {
            if (should_stop()) break;
            int window = (depth >= 5) ? 24 : INF;
            alpha = std::max(-INF, prevScore - window);
            beta = std::min(INF, prevScore + window);
            if (window == INF) {
                alpha = -INF;
                beta = INF;
            }

            std::vector<std::pair<int, Move>> scored;
            scored.reserve(rootMoves.size());
            std::vector<int> biases;
            biases.reserve(rootMoves.size());

            bool reSearch = true;
            int passes = 0;
            while (reSearch && !should_stop()) {
                reSearch = false;
                scored.clear();
                biases.clear();
                for (std::size_t i = 0; i < rootMoves.size(); ++i) {
                    Move m = rootMoves[i];
                    Undo u;
                    pos.do_move(m, u);
                    int score;
                    if (i == 0) score = -search(depth - 1, 1, -beta, -alpha, true, m, false);
                    else {
                        score = -search(depth - 1, 1, -alpha - 1, -alpha, false, m, true);
                        if (score > alpha && score < beta) score = -search(depth - 1, 1, -beta, -alpha, true, m, false);
                    }
                    pos.undo_move(m, u);
                    if (ctrl->stop.load(std::memory_order_relaxed)) break;
                    scored.emplace_back(score, m);
                    biases.push_back(root_opening_bias(m));
                    if (score > alpha) alpha = score;
                }
                if (scored.empty()) break;
                std::vector<std::size_t> ord(scored.size());
                for (std::size_t i = 0; i < ord.size(); ++i) ord[i] = i;
                std::sort(ord.begin(), ord.end(), [&](std::size_t a, std::size_t b) {
                    if (scored[a].first != scored[b].first) return scored[a].first > scored[b].first;
                    if (biases[a] != biases[b]) return biases[a] > biases[b];
                    return a < b;
                });
                std::vector<std::pair<int, Move>> sorted;
                sorted.reserve(scored.size());
                std::vector<int> sortedBias;
                sortedBias.reserve(biases.size());
                for (std::size_t i : ord) {
                    sorted.push_back(scored[i]);
                    sortedBias.push_back(biases[i]);
                }
                scored.swap(sorted);
                biases.swap(sortedBias);
                int best = scored.front().first;
                if (window != INF && (best <= prevScore - window || best >= prevScore + window) && passes < 3) {
                    int grow = 20 + 24 * passes;
                    alpha = best - window - grow;
                    beta = best + window + grow;
                    reSearch = true;
                    ++passes;
                } else {
                    prevScore = best;
                }
            }

            if (scored.empty()) break;
            rootMoves.clear();
            for (const auto& p : scored) rootMoves.push_back(p.second);

            ri.bestMove = scored.front().second;
            ri.bestScore = scored.front().first;
            ri.depth = depth;
            ri.seldepth = std::max(ri.seldepth, st.selDepth);
            ri.nodes = ctrl->globalNodes.load(std::memory_order_relaxed) + nodes;
            int pvl = st.pvLen[1];
            ri.pv = legalize_pv_from_root(ri.bestMove, st.pv[1], pvl);
            if (ri.pv.empty()) ri.pv.push_back(ri.bestMove);
            ri.ponderMove = (ri.pv.size() >= 2) ? ri.pv[1] : 0;

            if (outputInfo) {
                std::int64_t ms = std::max<std::int64_t>(1, now_ms());
                std::uint64_t n = ctrl->globalNodes.load(std::memory_order_relaxed) + nodes;
                std::uint64_t nps = (n * 1000ULL) / std::uint64_t(ms);
                int mpv = std::max(1, std::min(params.multiPV, (int) scored.size()));
                for (int i = 0; i < mpv; ++i) {
                    std::cout << "info depth " << depth
                              << " seldepth " << st.selDepth
                              << " multipv " << (i + 1)
                              << " score " << score_to_uci(scored[i].first)
                              << " nodes " << n
                              << " nps " << nps
                              << " hashfull " << tt->hashfull_permille()
                              << " tbhits 0"
                              << " time " << ms
                              << " pv " << move_to_uci(scored[i].second);
                    if (i == 0) {
                        for (std::size_t j = 1; j < ri.pv.size(); ++j) std::cout << " " << move_to_uci(ri.pv[j]);
                    }
                    std::cout << "\n";
                }
                std::cout.flush();
            }

            if (!ctrl->limits.infinite && ctrl->softMs > 0 && now_ms() >= ctrl->softMs && depth >= 2) break;
            if (std::abs(ri.bestScore) > MATE_SCORE - MAX_PLY) break;
        }
        ctrl->globalNodes.fetch_add(nodes, std::memory_order_relaxed);
        return ri;
    }

    static std::string move_to_uci(Move m) {
        if (!m) return "0000";
        std::string s = square_to_string(move_from(m)) + square_to_string(move_to(m));
        int promo = move_promo(m);
        if (promo) {
            char c = 'q';
            if (promo == 1) c = 'n';
            else if (promo == 2) c = 'b';
            else if (promo == 3) c = 'r';
            s.push_back(c);
        }
        return s;
    }
};

std::string move_to_uci(Move m) {
    if (!m) return "0000";
    std::string s = square_to_string(move_from(m)) + square_to_string(move_to(m));
    int promo = move_promo(m);
    if (promo) {
        char c = 'q';
        if (promo == 1) c = 'n';
        else if (promo == 2) c = 'b';
        else if (promo == 3) c = 'r';
        s.push_back(c);
    }
    return s;
}

struct UciOption {
    enum Type : std::uint8_t { Spin, Check, String, Button };
    Type type = Spin;
    int spin = 0;
    int min = 0;
    int max = 0;
    bool check = false;
    std::string str;
    int defaultSpin = 0;
    bool defaultCheck = false;
    std::string defaultStr;
};

struct Engine {
    Position pos;
    TranspositionTable tt;
    SearchParams params;
    std::unordered_map<std::string, UciOption> options;
    std::thread searchThread;
    mutable std::mutex ioMutex;
    SearchControl ctrl;
    std::atomic<bool> searching{false};

    Engine() {
        pos.set_startpos();
        tt.resize_mb(params.hashMB);
        init_options();
    }

    ~Engine() {
        stop_search();
    }

    static std::string lower(std::string s) {
        std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
            return char(std::tolower(c));
        });
        return s;
    }

    void init_options() {
        auto add_spin = [&](const std::string& n, int d, int mn, int mx) {
            UciOption o;
            o.type = UciOption::Spin;
            o.spin = d;
            o.defaultSpin = d;
            o.min = mn;
            o.max = mx;
            options[lower(n)] = o;
        };
        auto add_check = [&](const std::string& n, bool d) {
            UciOption o;
            o.type = UciOption::Check;
            o.check = d;
            o.defaultCheck = d;
            options[lower(n)] = o;
        };
        auto add_string = [&](const std::string& n, const std::string& d) {
            UciOption o;
            o.type = UciOption::String;
            o.str = d;
            o.defaultStr = d;
            options[lower(n)] = o;
        };
        auto add_button = [&](const std::string& n) {
            UciOption o;
            o.type = UciOption::Button;
            options[lower(n)] = o;
        };

        add_spin("Threads", 1, 1, static_cast<int>(std::max(1u, std::thread::hardware_concurrency())));
        add_spin("Hash", 16, 1, 65536);
        add_check("Ponder", false);
        add_spin("MultiPV", 1, 1, 8);
        add_check("UCI_Chess960", false);
        add_spin("MoveOverhead", 20, 0, 5000);
        add_string("SyzygyPath", "");
        add_spin("SyzygyProbeDepth", 1, 1, 64);
        add_spin("SyzygyProbeLimit", 7, 0, 7);
        add_button("Clear Hash");
    }

    int get_spin(const std::string& name) const {
        auto it = options.find(lower(name));
        if (it == options.end()) return 0;
        return it->second.spin;
    }
    bool get_check(const std::string& name) const {
        auto it = options.find(lower(name));
        if (it == options.end()) return false;
        return it->second.check;
    }
    std::string get_string(const std::string& name) const {
        auto it = options.find(lower(name));
        if (it == options.end()) return {};
        return it->second.str;
    }

    void sync_params_from_options() {
        params.threads = get_spin("Threads");
        params.hashMB = get_spin("Hash");
        params.ponder = get_check("Ponder");
        params.multiPV = get_spin("MultiPV");
        params.chess960 = get_check("UCI_Chess960");
        params.moveOverhead = get_spin("MoveOverhead");
        params.syzygyPath = get_string("SyzygyPath");
        params.syzygyProbeDepth = get_spin("SyzygyProbeDepth");
        params.syzygyProbeLimit = get_spin("SyzygyProbeLimit");
    }

    static void print_uci_options() {
        std::cout << "option name Threads type spin default 1 min 1 max "
                  << std::max(1u, std::thread::hardware_concurrency()) << "\n";
        std::cout << "option name Hash type spin default 16 min 1 max 65536\n";
        std::cout << "option name Ponder type check default false\n";
        std::cout << "option name MultiPV type spin default 1 min 1 max 8\n";
        std::cout << "option name UCI_Chess960 type check default false\n";
        std::cout << "option name MoveOverhead type spin default 20 min 0 max 5000\n";
        std::cout << "option name SyzygyPath type string default\n";
        std::cout << "option name SyzygyProbeDepth type spin default 1 min 1 max 64\n";
        std::cout << "option name SyzygyProbeLimit type spin default 7 min 0 max 7\n";
        std::cout << "option name Clear Hash type button\n";
    }

    Move parse_uci_move(const std::string& s) {
        MoveList ml;
        pos.generate_legal(ml, false);
        for (int i = 0; i < ml.size; ++i) {
            Move m = ml.moves[i];
            if (move_to_uci(m) == s) return m;
        }
        return 0;
    }

    void set_position_from_command(std::istringstream& is) {
        std::string token;
        if (!(is >> token)) return;
        if (token == "startpos") {
            pos.set_startpos();
        } else if (token == "fen") {
            std::string fen;
            std::string part;
            for (int i = 0; i < 6; ++i) {
                if (!(is >> part)) return;
                fen += part;
                if (i != 5) fen += ' ';
            }
            pos.set_fen(fen);
        } else {
            return;
        }

        std::string movesTok;
        if (!(is >> movesTok)) return;
        if (movesTok != "moves") return;
        while (is >> movesTok) {
            Move m = parse_uci_move(movesTok);
            if (!m) break;
            Undo u;
            pos.do_move(m, u);
        }
    }

    TimeBudget compute_time_budget(const Limits& lim) const {
        TimeBudget tb;
        if (lim.infinite) return tb;
        if (lim.movetime > 0) {
            std::int64_t mt = std::max<std::int64_t>(1, lim.movetime - std::max(5, params.moveOverhead / 2));
            tb.softMs = mt;
            tb.hardMs = mt + 5;
            return tb;
        }

        int remain = (pos.sideToMove == WHITE) ? lim.wtime : lim.btime;
        int inc = (pos.sideToMove == WHITE) ? lim.winc : lim.binc;
        if (remain <= 0) return tb;

        std::int64_t reserve = std::max<std::int64_t>(5, params.moveOverhead);
        std::int64_t usable = std::max<std::int64_t>(1, remain - reserve);
        int mtg = lim.movestogo > 0 ? lim.movestogo : 20;

        double alloc = (double(usable) / mtg) + 0.8 * inc;
        std::int64_t soft = std::max<std::int64_t>(5, std::min<std::int64_t>(usable, (std::int64_t) std::llround(alloc)));
        std::int64_t hard = std::max<std::int64_t>(soft + 5, std::min<std::int64_t>(usable, soft * 2 + 25));

        tb.softMs = soft;
        tb.hardMs = hard;
        return tb;
    }

    void launch_search(const Limits& lim) {
        stop_search();
        sync_params_from_options();
        tt.resize_mb(std::size_t(params.hashMB));
        tt.age.fetch_add(1, std::memory_order_relaxed);

        ctrl.stop.store(false, std::memory_order_relaxed);
        ctrl.pondering.store(params.ponder, std::memory_order_relaxed);
        ctrl.globalNodes.store(0, std::memory_order_relaxed);
        ctrl.startTime = Clock::now();
        ctrl.limits = lim;
        TimeBudget tb = compute_time_budget(lim);
        ctrl.softMs = tb.softMs;
        ctrl.hardMs = tb.hardMs;

        Position root = pos;
        searching.store(true, std::memory_order_relaxed);
        searchThread = std::thread([this, root]() mutable {
            int nThreads = std::max(1, params.threads);
            std::vector<RootInfo> results(nThreads);
            std::vector<std::thread> workers;
            workers.reserve(nThreads);
            for (int i = 0; i < nThreads; ++i) {
                workers.emplace_back([&, i]() {
                    Searcher s(root, &tt, &ctrl, params, i, i == 0);
                    results[i] = s.iterative_deepening();
                });
            }
            for (auto& th : workers) th.join();

            RootInfo best = results[0];
            for (int i = 1; i < nThreads; ++i) {
                if (results[i].depth > best.depth
                    || (results[i].depth == best.depth && results[i].bestScore > best.bestScore))
                    best = results[i];
            }
            std::lock_guard<std::mutex> lk(ioMutex);
            std::cout << "bestmove " << move_to_uci(best.bestMove);
            if (best.ponderMove) std::cout << " ponder " << move_to_uci(best.ponderMove);
            std::cout << "\n" << std::flush;
            searching.store(false, std::memory_order_relaxed);
        });
    }

    void stop_search() {
        if (!searching.load(std::memory_order_relaxed) && !searchThread.joinable()) return;
        ctrl.stop.store(true, std::memory_order_relaxed);
        if (searchThread.joinable()) searchThread.join();
        searching.store(false, std::memory_order_relaxed);
    }

    void set_option(const std::string& nameRaw, const std::string& valueRaw) {
        std::string name = lower(nameRaw);
        auto it = options.find(name);
        if (it == options.end()) return;
        UciOption& o = it->second;
        if (o.type == UciOption::Spin) {
            int v = o.spin;
            try {
                v = std::stoi(valueRaw);
            } catch (...) {
                return;
            }
            v = std::max(o.min, std::min(o.max, v));
            o.spin = v;
            if (name == "hash") tt.resize_mb(std::size_t(v));
        } else if (o.type == UciOption::Check) {
            std::string l = lower(valueRaw);
            o.check = (l == "true" || l == "1");
        } else if (o.type == UciOption::String) {
            o.str = valueRaw;
        } else if (o.type == UciOption::Button) {
            if (name == "clear hash") tt.clear();
        }
        sync_params_from_options();
    }

    void print_board() const {
        std::lock_guard<std::mutex> lk(ioMutex);
        for (int r = 7; r >= 0; --r) {
            std::cout << (r + 1) << "  ";
            for (int f = 0; f < 8; ++f) {
                int sq = r * 8 + f;
                Piece p = pos.board[sq];
                char c = '.';
                switch (p) {
                case W_PAWN: c = 'P'; break;
                case W_KNIGHT: c = 'N'; break;
                case W_BISHOP: c = 'B'; break;
                case W_ROOK: c = 'R'; break;
                case W_QUEEN: c = 'Q'; break;
                case W_KING: c = 'K'; break;
                case B_PAWN: c = 'p'; break;
                case B_KNIGHT: c = 'n'; break;
                case B_BISHOP: c = 'b'; break;
                case B_ROOK: c = 'r'; break;
                case B_QUEEN: c = 'q'; break;
                case B_KING: c = 'k'; break;
                default: break;
                }
                std::cout << c << ' ';
            }
            std::cout << "\n";
        }
        std::cout << "\n   a b c d e f g h\n";
        std::cout << "Fen: " << pos.fen() << "\n";
    }

    std::uint64_t perft(int depth) {
        if (depth <= 0) return 1;
        MoveList ml;
        pos.generate_legal(ml, false);
        if (depth == 1) return (std::uint64_t) ml.size;
        std::uint64_t nodes = 0;
        for (int i = 0; i < ml.size; ++i) {
            Undo u;
            pos.do_move(ml.moves[i], u);
            nodes += perft(depth - 1);
            pos.undo_move(ml.moves[i], u);
        }
        return nodes;
    }

    void run_bench() {
        std::vector<std::string> fens = {
          "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
          "r3k2r/p1ppqpb1/bn2pnp1/2pP4/1p2P3/2N2N2/PPQBBPPP/R3K2R w KQkq - 0 1",
          "4rrk1/2p1qppp/p1np1n2/2b1p3/2B1P3/2NP1N1P/PPQ2PP1/2KR1B1R w - - 0 1"
        };
        auto start = Clock::now();
        std::uint64_t nodes = 0;
        for (const auto& fen : fens) {
            pos.set_fen(fen);
            nodes += perft(5);
        }
        auto ms = std::max<std::int64_t>(
          1, std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count());
        std::lock_guard<std::mutex> lk(ioMutex);
        std::cout << "info string bench nodes " << nodes << " time " << ms << " nps " << (nodes * 1000ULL / ms) << "\n";
    }
};

Limits parse_go(std::istringstream& is) {
    Limits lim;
    std::string tok;
    while (is >> tok) {
        if (tok == "wtime") is >> lim.wtime;
        else if (tok == "btime") is >> lim.btime;
        else if (tok == "winc") is >> lim.winc;
        else if (tok == "binc") is >> lim.binc;
        else if (tok == "movestogo") is >> lim.movestogo;
        else if (tok == "depth") is >> lim.depth;
        else if (tok == "nodes") is >> lim.nodes;
        else if (tok == "movetime") is >> lim.movetime;
        else if (tok == "infinite") lim.infinite = true;
    }
    return lim;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    init_attack_tables();
    init_zobrist();

    Engine engine;
    engine.sync_params_from_options();

    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) continue;
        std::istringstream is(line);
        std::string token;
        is >> token;

        if (token == "uci") {
            std::cout << "id name " << kEngineName << "\n";
            std::cout << "id author " << kEngineAuthor << "\n";
            engine.print_uci_options();
            std::cout << "uciok\n" << std::flush;
        } else if (token == "isready") {
            std::cout << "readyok\n" << std::flush;
        } else if (token == "ucinewgame") {
            engine.stop_search();
            engine.tt.clear();
            engine.pos.set_startpos();
        } else if (token == "position") {
            engine.stop_search();
            engine.set_position_from_command(is);
        } else if (token == "setoption") {
            std::string t, name, value;
            is >> t;
            if (t != "name") continue;
            std::vector<std::string> nameParts, valueParts;
            while (is >> t) {
                if (t == "value") break;
                nameParts.push_back(t);
            }
            while (is >> t) valueParts.push_back(t);
            for (std::size_t i = 0; i < nameParts.size(); ++i) {
                if (i) name += ' ';
                name += nameParts[i];
            }
            for (std::size_t i = 0; i < valueParts.size(); ++i) {
                if (i) value += ' ';
                value += valueParts[i];
            }
            if (name.empty()) continue;
            engine.stop_search();
            engine.set_option(name, value);
        } else if (token == "go") {
            Limits lim = parse_go(is);
            engine.launch_search(lim);
        } else if (token == "stop") {
            engine.stop_search();
        } else if (token == "ponderhit") {
            // Ponder mode hook reserved for future extension.
        } else if (token == "quit") {
            engine.stop_search();
            break;
        } else if (token == "d") {
            engine.print_board();
        } else if (token == "eval") {
            int cp = evaluate(engine.pos);
            std::cout << "info string eval cp " << cp << "\n" << std::flush;
        } else if (token == "perft") {
            int depth = 1;
            is >> depth;
            auto start = Clock::now();
            std::uint64_t nodes = engine.perft(depth);
            auto ms = std::max<std::int64_t>(
              1, std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count());
            std::cout << "info string perft depth " << depth << " nodes " << nodes << " time " << ms
                      << " nps " << (nodes * 1000ULL / ms) << "\n" << std::flush;
        } else if (token == "bench") {
            engine.run_bench();
        }
    }
    return 0;
}
