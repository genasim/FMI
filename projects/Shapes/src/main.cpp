#include <exception>
#include <fstream>
#include <iostream>
#include <string>

#include "Circle.h"
#include "Group.h"
#include "Rect.h"
#include "Triangle.h"

#include "visitors/Serializer.h"

int main() {
    {
        Group* area =
            new Group({new Circle({1, 8}, 3.23), new Rect({1, 8}, {2, 5}),
                       new Triangle({0, 0}, {4, 0}, {1, 2})});

        Group* group = new Group();
        group->add(new Rect);
        // BUG \|/ For some reason calls rvalue overload; should call lvalue and
        // copy ptr data
        group->add(area);
        group->add(new Circle);
        group->add(new Triangle);

        std::cout << group->getArea() << std::endl;
        std::cout << group->getPerimeter() << std::endl;

        std::ofstream out("src/assets/shapes.txt");
        if (!out.is_open()) throw std::bad_exception();

        Serializer consoleWriter(std::cout);
        Serializer fileWriter(out);

        group->accept(consoleWriter);
        group->accept(fileWriter);

        // delete area; -> should delete it but ptr is beign added to the group
        // (not copied)
        //              => thus group becomes owner and deletes it;
        delete group;
    }
    std::cout << std::endl
              << "==============================" << std::endl
              << std::endl;
    {
        std::ifstream in("src/assets/shapes.txt");
        if (!in.is_open()) throw std::bad_exception();

        std::string type;
        in >> type;
        Shape* shape = Shape::factory(type.c_str());
        shape->deserialize(in);

        std::cout << shape->getArea() << std::endl;
        std::cout << shape->getPerimeter() << std::endl;
        delete shape;
    }

    return 0;
}
