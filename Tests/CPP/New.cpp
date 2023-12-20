#include <mcl.h>
#include <stdlib.h>

class Point {
public:
	Point(int nx, int ny) {
		x = nx;
		y = ny;
	}

private:
	int x;
	int y;
};

MCL_EXPORT(Call, Int, x, Int, y, CDecl_Ptr)
Point* Call(int x, int y) {
	return new Point(x, y);
}