package bg.sofia.uni.fmi.mjt.order.loader.exceptions;

public class CouldNotReadDatasetLineException extends RuntimeException {
    public CouldNotReadDatasetLineException(String message) {
        super(message);
    }

    public CouldNotReadDatasetLineException(String message, Throwable cause) {
        super(message, cause);
    }
}
