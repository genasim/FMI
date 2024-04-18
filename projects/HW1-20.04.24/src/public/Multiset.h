#pragma once
#include <cmath>
#include <fstream>
#include <iostream>

class Multiset {
   public:
    Multiset(size_t groupSize, size_t MAX_NUM);
    Multiset(uint8_t* data, size_t groupSize, size_t MAX_NUM);
    ~Multiset();

    void add(uint32_t num);
    void remove(uint32_t num);
    int contains(uint32_t num) const;

    void outputBinary(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& out, const Multiset& set);

    void serialize(std::ostream&);
    void deserialize(std::ostream&);

   private:
    uint8_t* data;
    size_t size;
    uint8_t groupSize;
    size_t MAX_NUM;

    struct Node {
        Node(unsigned int num, size_t occ) : num(num), occurrences(occ) {}
        unsigned int num;
        size_t occurrences;
    };
    friend std::ostream& operator<<(std::ostream&, const Multiset::Node&);

    class MultisetBitIterator {
       public:
        MultisetBitIterator(uint8_t* array, size_t groupSize, size_t currNum,
                            size_t MAX_NUM);

        MultisetBitIterator& operator++();
        Node operator*() const;
        bool operator==(const MultisetBitIterator& other) const;
        bool operator!=(const MultisetBitIterator& other) const;

        void setOccurrences(uint8_t to);

       private:
        uint8_t* data;
        size_t groupSize;
        size_t MAX_NUM;
        unsigned int currNum;

        size_t byteIndex;
        size_t bitOffset;

        uint32_t extractOccurenceBits() const;
        uint8_t extractBits(uint8_t num, int end);
        void turnBitsOff(uint8_t& num, size_t start, size_t end);
    };

   public:
    MultisetBitIterator begin() const;
    MultisetBitIterator end() const;
};
