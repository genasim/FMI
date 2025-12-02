package bg.sofia.uni.fmi.mjt.imagekit;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.ImageAlgorithm;
import bg.sofia.uni.fmi.mjt.imagekit.cli.AlgorithmType;
import bg.sofia.uni.fmi.mjt.imagekit.filesystem.FileSystemImageManager;
import bg.sofia.uni.fmi.mjt.imagekit.filesystem.LocalFileSystemImageManager;

import java.awt.image.BufferedImage;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Main {
    static void main(String[] args) {
        if (args.length != 4 || !"--algo".equals(args[2])) {
            System.err.println("Usage: imagekit <input> <output> --algo <grayscale | edges>");
            System.exit(1);
        }

        Path input = Paths.get(args[0]);
        Path output = Paths.get(args[1]);

        FileSystemImageManager fileManager = new LocalFileSystemImageManager();
        BufferedImage image = null;
        try {
            image = fileManager.loadImage(input.toFile());
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
            System.exit(3);
        } catch (Exception e) {
            System.err.println("Error processing image: " + e.getMessage());
            e.printStackTrace();
            System.exit(2);
        }

        AlgorithmType algorithmType = parseAlgorithmType(args[3]);
        ImageAlgorithm imageAlgorithm = ImageAlgorithm.create(algorithmType);

        BufferedImage processedImage = imageAlgorithm.process(image);

        try {
            fileManager.saveImage(processedImage, output.toFile());
            System.out.println("Wrote processed image to " + output.toFile().getAbsolutePath());
        } catch (FileNotFoundException e) {
            System.err.println("Could not write processed image to " + output.toFile().getAbsolutePath());
            System.err.println("Parent directory does not exist.");
        } catch (FileAlreadyExistsException e) {
            System.err.println("Output file already exists: " + output.toFile().getAbsolutePath());
        } catch (IOException e) {
            System.err.println("Error writing processed image: " + e.getMessage());
        }
    }

    private static AlgorithmType parseAlgorithmType(String arg) {
        return switch (arg.toLowerCase()) {
            case "grayscale" -> AlgorithmType.GRAYSCALE;
            case "edges" -> AlgorithmType.EDGES;
            default -> {
                System.err.println("Invalid algorithm type: " + arg);
                System.err.println("Supported types: grayscale, edges");
                System.exit(1);
                yield null;
            }
        };
    }
}
