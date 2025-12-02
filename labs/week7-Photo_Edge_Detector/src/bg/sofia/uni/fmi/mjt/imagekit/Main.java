package bg.sofia.uni.fmi.mjt.imagekit;

import bg.sofia.uni.fmi.mjt.imagekit.filesystem.LocalFileSystemImageManager;

import java.io.FileNotFoundException;
import java.nio.file.Path;

public class Main {
    static void main(String[] args) {
//        if (args.length != 2) {
//            System.err.println("Usage: imagekit <input-path> <output-path>");
//            System.exit(1);
//        }
//
//        Path input = Paths.get(args[0]);
//        Path output = Paths.get(args[1]);

        Path input = Path.of("./resources/car.jpeg");

        var fileManager = new LocalFileSystemImageManager();

        try {
            // TODO: call your actual image processing here
            // Example placeholder (commented so it compiles):
            // EdgeDetector detector = new EdgeDetector();
            // BufferedImage result = detector.process(input);
            // ImageIO.write(result, "png", output.toFile());

            fileManager.loadImage(input.toFile());

        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
            System.exit(3);
        } catch (Exception e) {
            System.err.println("Error processing image: " + e.getMessage());
            e.printStackTrace();
            System.exit(2);
        }
    }
}
