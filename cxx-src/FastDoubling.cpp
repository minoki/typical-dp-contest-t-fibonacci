// Compile with -std=c++14
#include <cstdint>
#include <iostream>
#include <vector>
#include <algorithm> // std::copy, std::transform, std::fill_n
#include <numeric> // std::accumulate

const std::int64_t modulo = 1000000007LL;

using Poly = std::vector<std::int64_t>;

void reduce(int k, Poly& v)
{
    for (;;) {
        while (v.back() == 0) {
            v.pop_back();
        }
        if (v.size() <= k) {
            return;
        }
        auto b = v.back();
        auto l = v.size();
        for (int i = l - k - 1; i < l - 1; ++i) {
            v.at(i) = (v.at(i) + b) % modulo;
        }
        v.pop_back();
    }
}

auto mulP(int k, Poly const& v, Poly const& w) -> Poly
{
    int vl = v.size(), wl = w.size();
    Poly res(vl + wl - 1);
    for (int i = 0; i < vl + wl - 1; ++i) {
        int jmax = std::min(wl - 1, i);
        std::int64_t acc = 0;
        for (int j = std::max(0, i - vl + 1); j <= jmax; ++j) {
            acc += (v.at(i-j) * w.at(j)) % modulo;
            acc %= modulo;
        }
        res.at(i) = acc;
    }
    reduce(k, res);
    return res;
}

auto mulByX(int k, Poly const& v) -> Poly
{
    if (v.size() == k) {
        auto v_k = v.back();
        Poly w(k);
        w.front() = v_k;
        std::transform(v.begin(), v.end() - 1, w.begin() + 1, [=](std::int64_t x) { return (x + v_k) % modulo; });
        return w;
    } else {
        Poly w(v.size() + 1);
        w.front() = 0;
        std::copy(v.begin(), v.end(), w.begin() + 1);
        return w;
    }
}

auto powX(int k, std::int64_t n) -> Poly
{
    if (n == 0) {
        return {1};
    } else if (n == 1) {
        return {0, 1};
    } else {
        auto f = powX(k, n >> 1);
        if ((n & 1) == 0) {
            return mulP(k, f, f);
        } else {
            return mulByX(k, mulP(k, f, f));
        }
    }
}

int main()
{
    int k;
    std::int64_t n;
    std::cin >> k >> n;
    if (n <= k) {
        std::cout << 1 << std::endl;
    } else {
        auto f = powX(k, n - k);
        std::vector<std::int64_t> seq(2 * k - 1);
        std::fill_n(seq.begin(), k, 1);
        for (int i = 0; i < k - 1; ++i) {
            seq.at(i + k) = std::accumulate(seq.begin() + i, seq.begin() + i + k, 0, [](std::int64_t a, std::int64_t b) { return (a + b) % modulo; });
        }
        std::int64_t acc = 0;
        for (int i = 0; i < f.size(); ++i) {
            acc += (f.at(i) * seq.at(i + k - 1)) % modulo;
            acc %= modulo;
        }
        std::cout << acc << std::endl;
    }
}
