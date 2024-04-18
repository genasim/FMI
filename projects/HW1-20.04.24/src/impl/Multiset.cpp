#include "Multiset.h"

#include <bitset>
#include <cmath>
#include <cstring>

Multiset::Multiset(size_t groupSize, size_t MAX_NUM)
    : groupSize(groupSize), MAX_NUM(MAX_NUM) {
    size_t size = std::ceil((float)(MAX_NUM + 1) * groupSize / 8);
    this->size = size;

    this->data = new uint8_t[size];
    std::memset(this->data, 0, size);
}

Multiset::Multiset(uint8_t* data, size_t groupSize, size_t MAX_NUM)
    : groupSize(groupSize), MAX_NUM(MAX_NUM) {
    size_t size = std::ceil((float)(MAX_NUM + 1) * groupSize / 8);
    this->size = size;

    this->data = new uint8_t[size];
    strcpy((char*)this->data, (char*)data);
}

Multiset::~Multiset() { delete[] data; }

std::ostream& operator<<(std::ostream& out, const Multiset& set) {
    out << "[ ";
    for (auto num : set) {
        if (num.occurrences > 0) {
            out << num << ' ';
        }
    }
    out << "]";
    return out;
}

std::ostream& operator<<(std::ostream& out, const Multiset::Node& node) {
    out << "{ " << node.num << " -> x" << node.occurrences << " }";
    return out;
}

Multiset::MultisetBitIterator::MultisetBitIterator(uint8_t* array,
                                                   size_t groupSize,
                                                   size_t currNum,
                                                   size_t MAX_NUM)
    : data(array), groupSize(groupSize), currNum(currNum), MAX_NUM(MAX_NUM) {
    byteIndex = ((MAX_NUM + 1 - currNum) * groupSize) / 8;
    if (currNum == 0) --byteIndex;
    bitOffset = (8 - ((MAX_NUM + 1 - currNum) * groupSize) % 8) % 8;
}

Multiset::MultisetBitIterator& Multiset::MultisetBitIterator::operator++() {
    ++currNum;
    if (bitOffset + groupSize <= 8) {
        bitOffset += groupSize;
    } else {
        --byteIndex;
        bitOffset += groupSize - 8;
    }
    return *this;
}

bool Multiset::MultisetBitIterator::operator!=(
    const Multiset::MultisetBitIterator& other) const {
    return !(*this == other);
}

bool Multiset::MultisetBitIterator::operator==(
    const MultisetBitIterator& other) const {
    return (currNum == other.currNum);
}

size_t min(const size_t& a, const size_t& b) { return a < b ? a : b; }

uint32_t Multiset::MultisetBitIterator::extractOccurenceBits() const {
    uint32_t result = 0;
    uint8_t mask, maskOffset = 0;
    uint8_t tmpByteIndex = byteIndex, tmpBitOffset = bitOffset,
            bitsToExtract = groupSize;
    while (bitsToExtract != 0) {
        char extracted = min(bitsToExtract, 8 - tmpBitOffset);
        mask = (1 << extracted) - 1;
        result |= ((data[tmpByteIndex] >> tmpBitOffset) & mask) << maskOffset;

        tmpByteIndex -= 1;
        maskOffset += 8 - tmpBitOffset;
        tmpBitOffset = 0;
        bitsToExtract -= extracted;
    }

    return result;
}

Multiset::Node Multiset::MultisetBitIterator::operator*() const {
    uint32_t occurrences = extractOccurenceBits();
    return {currNum, occurrences};
}

Multiset::MultisetBitIterator Multiset::begin() const {
    return MultisetBitIterator(data, groupSize, 0, MAX_NUM);
}

Multiset::MultisetBitIterator Multiset::end() const {
    return MultisetBitIterator(data, groupSize, MAX_NUM + 1, MAX_NUM);
}

void Multiset::outputBinary(std::ostream& out) const {
    for (size_t i = 0; i < size; i++) {
        size_t offset = sizeof(data[0]) * 8 - 1;
        unsigned int mask = 1 << offset;
        while (mask >= 1) {
            bool bit = (data[i] & mask) != 0;
            out << bit;
            mask >>= 1;
        }
    }
    out << std::endl;
}

int Multiset::contains(uint32_t num) const {
    if (num > MAX_NUM) {
        throw std::out_of_range("Cannon search beyond MAX_NUM in multiset");
    }

    MultisetBitIterator it(data, groupSize, num, MAX_NUM);
    return (*it).occurrences;
}

void Multiset::add(uint32_t num) {
    MultisetBitIterator it(data, groupSize, num, MAX_NUM);
    size_t occurrences = (*it).occurrences;

    if (occurrences < std::pow(2, groupSize) - 1) {
        it.setOccurrences(occurrences + 1);
    }
}

void Multiset::remove(uint32_t num) {
    MultisetBitIterator it(data, groupSize, num, MAX_NUM);
    uint8_t occurrences = (*it).occurrences;

    if (occurrences > 0) {
        it.setOccurrences(occurrences - 1);
    }
}

void Multiset::MultisetBitIterator::turnBitsOff(uint8_t& num, size_t start,
                                                size_t end) {
    uint8_t mask = ~(uint8_t)(((1 << (end - start)) - 1) << start);
    num &= mask;
}

uint8_t Multiset::MultisetBitIterator::extractBits(uint8_t num, int endBit) {
    uint8_t mask = (1 << (endBit + 1)) - 1;
    uint8_t result = num & mask;

    return result;
}

void Multiset::MultisetBitIterator::setOccurrences(uint8_t to) {
    uint8_t tmpByteIndex = byteIndex, tmpBitOffset = bitOffset,
            remaining = groupSize;
    while (remaining != 0) {
        char processed = min(remaining, 8 - tmpBitOffset);
        char endIdx = min(tmpBitOffset + processed, 8);
        // std::cout << "byte: \t" << std::bitset<8>(data[tmpByteIndex]) <<
        // std::endl;
        turnBitsOff(data[tmpByteIndex], tmpBitOffset, endIdx);
        // std::cout << "byte: \t" << std::bitset<8>(data[tmpByteIndex]) <<
        // std::endl;
        uint8_t result = extractBits(to, processed) << tmpBitOffset;
        // std::cout << "result: " << std::bitset<8>(result) << std::endl;
        data[tmpByteIndex] |= result;
        // std::cout << "byte: \t" << std::bitset<8>(data[tmpByteIndex]) <<
        // std::endl;

        to >>= processed;
        tmpByteIndex -= 1;
        tmpBitOffset = 0;
        remaining -= processed;
    }
}