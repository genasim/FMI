package bg.sofia.uni.fmi.mjt.file.step;

import bg.sofia.uni.fmi.mjt.file.File;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CountFilesTest {
    @Test
    void testProcessNullCollectionThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> new CountFiles().process(null),
                     "Should throw IllegalArgumentException when passing null collection");
    }

    @Test
    void testProcessReturnsCorrectCount() {
        List<File> files = List.of(new File("test1"), new File("test2"));
        int count = new CountFiles().process(files);

        assertEquals(2, count, "Wrong count returned");
    }
}
