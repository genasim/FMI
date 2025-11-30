package bg.sofia.uni.fmi.mjt.file.step;

import bg.sofia.uni.fmi.mjt.file.File;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SplitFileTest {
    @Test
    void testProcessNullFileThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> new SplitFile().process(null),
                     "Should throw IllegalArgumentException when processing null file");
    }

    @Test
    void testProcessFileWithNullContentThrowsException() {
        File file = new File("test");
        file.setContent(null);

        assertThrows(IllegalArgumentException.class, () -> new SplitFile().process(file),
                     "Should throw IllegalArgumentException when processing file with null content");
    }

    @Test
    void testProcessFileWithEmptyContentReturnsEmptySet() {
        File file = new File("test");
        file.setContent("");

        Set<File> output = new SplitFile().process(file);
        assertTrue(output.isEmpty(), "Should return empty set");
    }

    @Test
    void testProcessFileWithNonEmptyContentReturnsSetWithOneElement() {
        File file = new File("test");

        Set<File> output = new SplitFile().process(file);
        assertEquals(Set.of(new File("test")), output, "Should return set with one element");
    }

    @Test
    void testProcessFileWithUniqueWordsReturnsSetWithEachWord() {
        File file = new File("Lorem ipsum dolor sit");

        Set<File> output = new SplitFile().process(file);
        assertEquals(Set.of(new File("Lorem"), new File("ipsum"), new File("dolor"), new File("sit")), output,
                     "Should return set with each word");
    }

    @Test
    void testProcessFileWithMultipleSpacesReturnsSetWithEachWord() {
        File file = new File("Lorem  ipsum  dolor  sit");
        Set<File> output = new SplitFile().process(file);

        assertEquals(Set.of(new File("Lorem"), new File("ipsum"), new File("dolor"), new File("sit")), output,
                     "Should return set with each word");
    }

    @Test
    void testProcessFileWithDuplicateWordsReturnsOnlyUniqueWords() {
        File file = new File("Lorem ipusm Lorem ipusm hello");

        Set<File> output = new SplitFile().process(file);
        assertEquals(Set.of(new File("Lorem"), new File("ipusm"), new File("hello")), output,
                     "Should return set with unique words");
    }
}
