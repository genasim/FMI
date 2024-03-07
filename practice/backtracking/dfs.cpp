#include <iostream>

using namespace std;

const size_t rows = 5;
const size_t cols = 5;

bool isValidCoords(const int maze[rows][cols], const int x, const int y) {
    return maze[x][y] != 1
            && x >= 0
            && y >= 0
            && x < rows
            && y < cols;
}

char resolveSym(const int code) {
    switch (code) {
    case 1:
        return '+';
    case 2:
        return '.';
    case 3:
        return 'S'; 
    case 4:
        return 'X';   
    default:
        return ' ';
    }
}

void printMaze(const int maze[rows][cols]) {
    for (size_t i = 0; i < rows; i++) {
        for (size_t j = 0; j < cols; j++)
            cout << "| " << resolveSym(maze[i][j]) << " |";
        cout << endl;
    }
}

bool propagateMaze(int maze[rows][cols], bool visited[rows][cols], const int curX, const int curY, const int destX, const int destY) {
    if (!isValidCoords(maze, curX, curY) || visited[curX][curY])
        return false;

    visited[curX][curY] = true;

    if (curX == destX && curY == destY)
        return true;

    bool foundPath = propagateMaze(maze, visited, curX - 1, curY, destX, destY) ||
                     propagateMaze(maze, visited, curX, curY + 1, destX, destY) ||
                     propagateMaze(maze, visited, curX, curY - 1, destX, destY) ||
                     propagateMaze(maze, visited, curX + 1, curY, destX, destY);

    if (foundPath)
        maze[curX][curY] = 2;
    return foundPath;
}

void findPath(int maze[rows][cols], const int startX, const int startY, const int destX, const int destY) {
    if (!isValidCoords(maze, startX, startY) || !isValidCoords(maze, destX, destY))
        throw runtime_error("Invalid start or dest coordinates!");

    bool visited[rows][cols] { false };    
    propagateMaze(maze, visited, startX, startY, destX, destY);
    
    maze[startX][startY] = 3;
    maze[destX][destY] = 4;

    printMaze(maze);
}

int main(int argc, char *argv[]) {
    cout << argv[0] << endl;
    int maze[rows][cols] = {
        { 1 , 0 , 0 ,  0 , 1},
        { 0 , 0 , 0 ,  0 , 0},
        { 1 , 0 , 0 ,  0 , 0},
        { 0 , 0 , 0 ,  1 , 1},
        { 0 , 1 , 0 ,  0 , 0}
    };

    findPath(maze, 3, 0, 1, 4);

    return 0;
}