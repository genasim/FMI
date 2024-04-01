#include <iostream>
#include <fstream>
#include <cstring>

enum class TyreState {
    Soft, 
    Medium, 
    Hard, 
    Intern, 
    Wet
};

struct Tyres {
    TyreState state;
    double coef;
};

struct Bolid {
    char name[65];
    unsigned year;
    Tyres tyres;
    bool isInBox;
    unsigned maxLaps;
};

size_t getCSVRecords(std::ifstream &csvFile) {
    size_t records = 0;
    while (true) {
        char ch = csvFile.get();
        if (ch == '\n')
            records++;
        
        if (csvFile.eof())
            break;
    }
    csvFile.clear();
    csvFile.seekg(0, std::ios::beg); 

    return records;
}

bool stringToBool(const char *string) {
    if (strcmp(string, "true") == 0) return true;
    if (strcmp(string, "false") == 0) return false;
    throw std::runtime_error("String must be 'true' or 'false'"); 
}

const char* boolToString(const bool value) {
    return value ? "true" : "false";
}

const char* resolveTyreStateToString(const TyreState state) {
    switch (state) {
        case TyreState::Soft: return "soft";
        case TyreState::Medium: return "medium";
        case TyreState::Hard: return "hard";
        case TyreState::Intern: return "intern";
        case TyreState::Wet: return "wet";
        default: throw std::runtime_error("Unkown state in enum"); 
    }
}

unsigned resolveTyreTypeMaxLaps(TyreState state) {
    switch (state) {
        case TyreState::Soft: return 45;
        case TyreState::Medium: return 50;
        case TyreState::Hard: return 60;
        case TyreState::Intern: return 40;
        case TyreState::Wet: return 35;
        default: return 0;
    }
}

TyreState stringToTyreState(const char tyre[65]) {
    if (strcmp(tyre, "soft") == 0) return TyreState::Soft;
    if (strcmp(tyre, "medium") == 0) return TyreState::Medium;
    if (strcmp(tyre, "hard") == 0) return TyreState::Hard;
    if (strcmp(tyre, "intern") == 0) return TyreState::Intern;
    if (strcmp(tyre, "wet") == 0) return TyreState::Wet;
    throw std::runtime_error("Unkown state string"); 
}


void readBolid(Bolid &bolid, std::istream &input) {
    char tyreState[16];

    input.getline(bolid.name, 65, ',');
    
    input >> bolid.year;
    input.ignore();

    input.getline(tyreState, 16, ',');

    input >> bolid.tyres.coef;
    input.ignore();

    char boolean[10]; // some arbitrary buffer value,  greater than length of "false"
    input.getline(boolean, 10);
    bolid.isInBox = stringToBool(boolean);

    TyreState state = stringToTyreState(tyreState);
    bolid.tyres.state = state;
    bolid.maxLaps = resolveTyreTypeMaxLaps(state);
}

void sortBolids(Bolid bolids[], const size_t size)
{
    int i, j;
    Bolid key;
    for (i = 1; i < size; i++) {
        key = bolids[i];
        j = i - 1;
 
        while (j >= 0 && bolids[j].year > key.year) {
            bolids[j + 1] = bolids[j];
            j = j - 1;
        }
        bolids[j + 1] = key;
    }
}

void writeBolid(const Bolid &bolid, std::ostream &output) {
    output << bolid.name << ','
           << bolid.year << ','
           << resolveTyreStateToString(bolid.tyres.state) << ','
           << bolid.tyres.coef << ','
           << boolToString(bolid.isInBox) << std::endl;
}

int main() {
    std::ifstream bolidsFile("../files-sem.02/bolids.csv");
    if (!bolidsFile.is_open() || bolidsFile.bad())
        throw std::runtime_error("Could not open input file");

    size_t size = getCSVRecords(bolidsFile);

    Bolid* bolids = new Bolid[size];    
    for (size_t i = 0; i < size; i++) {
        readBolid(bolids[i], bolidsFile);
        writeBolid(bolids[i], std::cout);
    }

    sortBolids(bolids, size);

    std::ofstream bolidsFile_sorted("../files-sem.02/tmp/bolids-sorted.csv");
    if (!bolidsFile_sorted.is_open() || bolidsFile_sorted.bad())
        throw std::runtime_error("Could not open output file");
        
    for (size_t i = 0; i < size; i++) {
        writeBolid(bolids[i], bolidsFile_sorted);
    }
    
    bolidsFile.close();
    bolidsFile_sorted.close();
    delete[] bolids;
    return 0;
}