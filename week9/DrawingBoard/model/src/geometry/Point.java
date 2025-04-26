package geometry;

public class Point {
    private int[] coords; //(x; y)

    public Point() {
        this(new int[]{0, 0}); //new double[2]
    }

    public Point(int[] coords) {
        setCoords(coords);
    }

    public Point(Point p) {
        this(p.getCoords());
    }

    public int[] getCoords() {
        int[] copy = new int[coords.length];
        for (int i = 0; i < coords.length; i++) {
            copy[i] = coords[i];
        }
        return copy;
    }

    public void setCoords(int[] coords) {
        if (coords != null && coords.length == 2) {
            this.coords = new int[coords.length];
            for (int i = 0; i < coords.length; i++) {
                //possible if check
                this.coords[i] = coords[i];
            }
        } else {
            this.coords = new int[2];
        }
    }

    public Point getPoint() {
        return new Point(this);
    }

    public void setPoint(Point point) {
        if (point != null) {
            setCoords(point.coords);
        } else {
            setCoords(new int[]{0, 0});
        }
    }

    @Override
    public String toString() {
//        String result = "";
//        for (int i = 0; i < coords.length - 1; i++) {
//            result += coords[i] + ",";
//        }
//        result += coords[coords.length - 1];
        return String.format("(%d; %d)", coords[0], coords[1]);
    }
}
