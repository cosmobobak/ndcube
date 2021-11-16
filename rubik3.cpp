#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <span>
#include <sstream>
#include <string>

namespace cag {
// from https://stackoverflow.com/a/19023500/13243460
template <class Function, std::size_t... Indices>
constexpr auto make_array_helper(Function f, std::index_sequence<Indices...>)
    -> std::array<typename std::result_of<Function(std::size_t)>::type, sizeof...(Indices)> {
    return {{f(Indices)...}};
}

template <int N, class Function>
constexpr auto make_array(Function f)
    -> std::array<typename std::result_of<Function(std::size_t)>::type, N> {
    return make_array_helper(f, std::make_index_sequence<N>{});
}
}  // namespace cag

#define let const auto
#define loop while (true)

using coord_t = uint8_t;
using dim_t = uint8_t;

constexpr auto X = 0, Y = 1, Z = 2, W = 3;
constexpr auto RED = "\u001b[31m";
constexpr auto GREEN = "\u001b[32m";
constexpr auto RESET = "\u001b[0m";

constexpr int64_t ipow(int64_t base, int exp, int64_t result = 1) {
    return exp < 1 ? result : ipow(base * base, exp / 2, (exp % 2) ? result * base : result);
}

enum Side {
    FRONT = 0,
    BACK = 2,
};

struct Rotation {
    dim_t axis;
    dim_t from;
    dim_t to;
    Side side;

    template <dim_t DIMS>
    static auto random() -> Rotation {
        auto side = (rand() & 1) * 2;
        auto axis = rand() % DIMS;
        auto from = rand() % DIMS;
        while (from == axis) {
            from = rand() % DIMS;
        }
        auto to = rand() % DIMS;
        while (to == axis || to == from) {
            to = rand() % DIMS;
        }
        return Rotation{(dim_t)axis, (dim_t)from, (dim_t)to, (Side)side};
    }
};

template <dim_t DIMS>
struct Point {
    using vec = std::array<coord_t, DIMS>;
    vec original_coords = {0};
    vec coords = {0};
    vec orientation = {0};

    static auto create(vec input) -> Point {
        vec orientation;
        std::iota(orientation.begin(), orientation.end(), 0);
        return Point{input, input, orientation};
    }

    static auto from_index(size_t i) -> Point {
        vec vals;
        for (size_t index = 0; index < DIMS; ++index) {
            let dividend = i / (ipow(3, index));
            vals[index] = (dividend % 3);
        }
        return create(vals);
    }

    void rotate(const Rotation r) {
        let rotation_axis = r.axis;
        let from_axis = r.from;
        let to_axis = r.to;

        assert(rotation_axis != from_axis && from_axis != to_axis && to_axis != rotation_axis);
        assert(rotation_axis < DIMS && from_axis < DIMS && to_axis < DIMS);

        if (coords[rotation_axis] != r.side) {
            return;
        }

        // this is trivial
        std::swap(orientation[from_axis], orientation[to_axis]);

        // this is not
        switch (coords[from_axis]) {
            case 0:
                switch (coords[to_axis]) {
                    case 0:
                        coords[from_axis] = 2;
                        coords[to_axis] = 0;
                        break;
                    case 1:
                        coords[from_axis] = 1;
                        coords[to_axis] = 0;
                        break;
                    case 2:
                        coords[from_axis] = 0;
                        coords[to_axis] = 0;
                        break;
                    default:
                        assert(false);
                }
                break;
            case 1:
                switch (coords[to_axis]) {
                    case 0:
                        coords[from_axis] = 2;
                        coords[to_axis] = 1;
                        break;
                    case 1:
                        break;
                    case 2:
                        coords[from_axis] = 0;
                        coords[to_axis] = 1;
                        break;
                    default:
                        assert(false);
                }
                break;
            case 2:
                switch (coords[to_axis]) {
                    case 0:
                        coords[from_axis] = 2;
                        coords[to_axis] = 2;
                        break;
                    case 1:
                        coords[from_axis] = 1;
                        coords[to_axis] = 2;
                        break;
                    case 2:
                        coords[from_axis] = 0;
                        coords[to_axis] = 2;
                        break;
                    default:
                        assert(false);
                }
                break;
            default:
                assert(false);
        }
    }

    auto is_in_original_position() const -> bool {
        return coords == original_coords;
    }

    auto is_in_original_orientation() const -> bool {
        return std::is_sorted(orientation.begin(), orientation.end());
    }

    auto is_center() const -> bool {
        return std::count(coords.begin(), coords.end(), 1) == DIMS - 1;
    }

    auto to_string() const -> std::string {
        std::stringstream out;
        out << RESET;
        out << "Current coordinates: ";
        if (is_in_original_position()) {
            out << GREEN;
        } else {
            out << RED;
        }
        for (auto c : coords) out << (int)c << " ";
        out << RESET;
        out << "Orientation: ";
        if (is_in_original_orientation()) {
            out << GREEN;
        } else {
            out << RED;
        }
        for (auto c : orientation) out << (int)c << " ";
        out << RESET;
        out << "Original coordinates: ";
        for (auto c : original_coords) out << (int)c << " ";
        return out.str();
    }

    auto dist_from_original() const -> int {
        return std::transform_reduce(
            coords.begin(), coords.end(), 
            original_coords.begin(),
            0, std::plus{}, [](auto a, auto b) {
                return std::abs(a - b);
            });
    }

    auto incorrectness() const -> int {
        auto smallmod = is_in_original_orientation() ? 0 : 1;
        return dist_from_original() + smallmod * 10;
    }
};

template <dim_t DIMS>
struct Cube {
    constexpr static auto NUM_POINTS = ipow(3, DIMS);
    constexpr static std::array AXES = cag::make_array<DIMS>([](auto i) { return (dim_t)i; });
    std::array<Point<DIMS>, NUM_POINTS> points;

    Cube() {
        for (size_t idx = 0; idx < NUM_POINTS; ++idx) {
            points[idx] = Point<DIMS>::from_index(idx);
        }
    }

    void rotate(Rotation r) {
        std::for_each(
            points.begin(),
            points.end(),
            [this, r](auto& p) {
                p.rotate(r);
            });
    }

    void rotate_n(Rotation r, size_t n) {
        for (size_t i = 0; i < n; ++i) {
            rotate(r);
        }
    }

    void undo_rotation(Rotation r) {
        rotate_n(r, 3);
    }

    auto is_solved() const -> bool {
        return std::all_of(
            points.begin(),
            points.end(),
            [](Point<DIMS> p) {
                return p.is_in_original_position() && (p.is_in_original_orientation() || p.is_center());
            });
    }

    auto unsolvedness() const -> int {
        return std::transform_reduce(
            points.begin(), points.end(),
            0, std::plus{}, [](auto p) {
                return p.incorrectness();
            });
    }

    void show() const {
        std::cout << "Current state: " << std::endl;
        for (auto p : points) {
            std::cout << p.to_string() << std::endl;
        }
        std::cout << "Solved? " << (is_solved() ? "Yes" : "No") << std::endl;
        std::cout << "Unsolvedness: " << unsolvedness() << std::endl;
    }

    void shuffle(size_t times) {
        for (size_t i = 0; i < times; ++i) {
            rotate(Rotation::random<DIMS>());
        }
    }
    
    void solve() {
        std::vector<Rotation> rotations;
        while (!is_solved()) {
            let last_unsolvedness = unsolvedness();
            auto r = Rotation::random<DIMS>();
            rotate(r);
            rotations.push_back(r);
            let random_value = rand() % 100;
            let current_unsolvedness = unsolvedness();
            if (current_unsolvedness > last_unsolvedness) {
                if (random_value < 90) {
                    undo_rotation(r);
                    rotations.pop_back();
                }
            } else {
                if (random_value < 10) {
                    undo_rotation(r);
                    rotations.pop_back();
                }
            }
            std::cout << unsolvedness() << std::endl;
        }
        std::cout << "solved in " << rotations.size() << " rotations." << std::endl;
    }
};

auto not_in(dim_t e, const std::span<const dim_t> es) -> bool {
    return std::find(es.begin(), es.end(), e) == es.end();
}

auto split(const std::string& string, const char sep) {
    // consider switching to a search-and-allocate method if push_back becomes a performace bottleneck.
    std::vector<std::string> result;
    std::stringstream ss(string);
    while (ss.good()) {
        std::string substr;
        std::getline(ss, substr, sep);
        result.push_back(substr);
    }
    return result;
}

template <dim_t DIMS>
auto get_rots_from_user() -> std::vector<Rotation> {
    std::vector<Rotation> result;
    std::string input;
    std::cout << "Enter a rotation: ";
    std::cin >> input;
    let parts = split(input, ',');
    for (auto part : parts) {
        std::array<dim_t, 4> rotparts;
        auto i = 0;
        for (char c : part) {
            rotparts[i++] = (dim_t)c - '0';
        }
        result.push_back(Rotation{ rotparts[0], rotparts[1], rotparts[2], (Side)rotparts[3] });
    }
    return result;
}

constexpr auto INIT_DIMS = 3;

auto main() -> int {
    std::cout << "The N-D Cube (where N is currently " << INIT_DIMS << ")" << std::endl;
    std::cout << "Enter rotations in the form of four digits (like 1230), where" << std::endl;
    std::cout << " - the first digit is the axis to rotate around" << std::endl;
    std::cout << " - the second digit is the axis to rotate from" << std::endl;
    std::cout << " - the third digit is the axis to rotate to" << std::endl;
    std::cout << " - the fourth digit is the side to rotate [either 0 or 2]" << std::endl;
    std::cout << "For example, to rotate the top face clockwise" << std::endl;
    std::cout << " - we would be rotating around the Y axis (axis 1), " << std::endl;
    std::cout << " - from the Z axis (2), " << std::endl;
    std::cout << " - to the X axis (0), " << std::endl;
    std::cout << " - and we would be rotating the face \"further in the Y direction\" (higher up) (2). " << std::endl;
    std::cout << "So our command would be 1202." << std::endl;

    auto c = Cube<INIT_DIMS>();

    c.shuffle(100);

    c.show();

    loop {
        let rotations = get_rots_from_user<INIT_DIMS>();

        for (auto r : rotations) {
            c.rotate(r);
        }

        c.show();
    }

}