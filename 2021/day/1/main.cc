#include <iostream>
#include <vector>

using namespace std;

int main() {
	vector<int> meas;
	int n;
	while (cin >> n) meas.push_back(n);

	// Part 1
	int ans1 = 0;

	for (int i = 0; i < meas.size() - 1; i++) {
		int a = meas[i + 0];
		int b = meas[i + 1];
		ans1 += b > a;
	}

	cout << ans1 << endl;

	// Part 2
	int ans2 = 0;

	for (int i = 0; i < meas.size() - 3; i++) {
		int a = meas[i + 0] /* + meas[i + 1] + meas[i + 2] */;
		int b = /* meas[i + 1] + meas[i + 2] + */ meas[i + 3];
		ans2 += b > a;
	}

	cout << ans2 << endl;
}
