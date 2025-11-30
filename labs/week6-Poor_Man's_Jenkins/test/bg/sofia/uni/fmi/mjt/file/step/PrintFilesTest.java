package bg.sofia.uni.fmi.mjt.file.step;

import bg.sofia.uni.fmi.mjt.file.File;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PrintFilesTest {
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;

    @BeforeEach
    void setUp() {
        System.setOut(new PrintStream(outContent));
    }

    @AfterEach
    void tearDown() {
        System.setErr(originalOut);
    }

    @Test
    void testProcessNullInputCollectionThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> new PrintFiles().process(null),
                     "Should throw IllegalArgumentException when passing null collection");
    }

    @Test
    void testProcessPrintFilesToStdOut() {
        List<File> files = List.of(new File("test1"), new File("test2"));
        var printer = new PrintFiles();

        printer.process(files);
        String expected = "test1" + System.lineSeparator() + "test2" + System.lineSeparator();
        assertEquals(expected, outContent.toString(), "Output is not as expected");
    }

    @Test
    void testProcessSkipPrintingNullFilesToStdOut() {
        List<File> files = new ArrayList<>();
        files.add(null);
        files.add(new File("test2"));

        var printer = new PrintFiles();

        printer.process(files);
        assertEquals("test2" + System.lineSeparator(), outContent.toString(), "Should not print anything to stdout");
    }

    @Test
    void testProcessPrintFilesWithNullContentToStdOut() {
        var file = new File("test");
        file.setContent(null);
        var printer = new PrintFiles();

        printer.process(List.of(file));
        assertEquals("null" + System.lineSeparator(), outContent.toString(), "Output should be 'null' string");
    }

    @Test
    void testProcessReturnSameUnmodifiedCollection() {
        Collection<File> files = List.of(new File("test1"), new File("test2"));
        var printer = new PrintFiles();

        Collection<File> output = printer.process(files);

        assertEquals(files, output, "Should return the same collection");
        assertEquals(2, output.size(), "Should not modify the collection");
        assertSame(files, output, "Should return the same collection instance (not a copy");
    }
}
