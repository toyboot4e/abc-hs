#include <bits/stdc++.h>

constexpr unsigned long hilbert_order(unsigned long x, unsigned long y){
	unsigned long d{}, s{20};
	for(; s--;){
		unsigned long rx{1UL & (x >> s)}, ry{1UL & (y >> s)};
		(d <<= 2) |= rx * 3 ^ ry;
		if (!ry) {
			if (rx) {
				x = ~x;
				y = ~y;
			}
			std::swap(x, y);
		}
	}
	return d;
}

int main() {
    int l;
    int r;
    std::cin >> l >> r;
    printf("%d", hilbert_order(l, r));
    return 0;
}

