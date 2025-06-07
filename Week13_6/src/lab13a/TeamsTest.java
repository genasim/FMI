package lab13a;

import java.util.TreeMap;

public class TeamsTest {
    public static void main(String[] args) {
        TreeMap<String, String>
                teams = new TreeMap<>();
        teams.put("San Francisco", "Forty-niners");
        teams.put("Chicago", "Bears");
        teams.put("Denver", "Broncos");
        teams.put("Seattle", "Seahawks");
        teams.put("Miami", "Dolphins");
        teams.put("Detroit", "Lions");

        //a)
        System.out.println("Elements in teams:" + teams.size());
        System.out.println("Chicago team:" + teams.get("Chicago"));

        //b)
        teams.put("San Francisco", "Niners");

        //c)
        boolean hasTeam = teams.containsKey("San Diego");
        System.out.println("San Diego has a team:" +
                ((hasTeam)? "Yes" : "No"));

        //d)
        teams.remove("Denver");

        //e) & f)
        teams.put("Dallas", "Cowboys");
        System.out.println(teams);
        for (String town : teams.keySet()) {
            System.out.printf("%-15s: %-15s%n",town,teams.get(town));
        }

    }
}
