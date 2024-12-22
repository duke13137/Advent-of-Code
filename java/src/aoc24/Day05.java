package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Day05 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day05.txt");
        List<String> input = Files.readAllLines(inputPath);

        String rulesStr = String.join("\n", input.subList(0, input.indexOf("")));
        String updatesStr = String.join("\n", input.subList(input.indexOf("") + 1, input.size()));

        long part1Result = solve(rulesStr, updatesStr);
        System.out.println("Part 1: " + part1Result);

        long part2Result = solvePart2(rulesStr, updatesStr);
        System.out.println("Part 2: " + part2Result);
    }

    public static List<List<Long>> parseRules(String rulesStr) {
        List<List<Long>> rules = new ArrayList<>();
        String[] lines = rulesStr.split("\n");
        for (String line : lines) {
            String[] parts = line.split("\\|");
            List<Long> rule = new ArrayList<>();
            rule.add(Long.parseLong(parts[0]));
            rule.add(Long.parseLong(parts[1]));
            rules.add(rule);
        }
        return rules;
    }

    public static List<Long> parseUpdate(String updateStr) {
        List<Long> update = new ArrayList<>();
        String[] parts = updateStr.split(",");
        for (String part : parts) {
            update.add(Long.parseLong(part));
        }
        return update;
    }

    public static List<List<Long>> parseUpdates(String updatesStr) {
        List<List<Long>> updates = new ArrayList<>();
        String[] lines = updatesStr.split("\n");
        for (String line : lines) {
            updates.add(parseUpdate(line));
        }
        return updates;
    }

    public static List<List<Long>> applicableRules(List<Long> update, List<List<Long>> rules) {
        List<List<Long>> appRules = new ArrayList<>();
        for (List<Long> rule : rules) {
            if (update.contains(rule.get(0)) && update.contains(rule.get(1))) {
                appRules.add(rule);
            }
        }
        return appRules;
    }

    public static boolean validateUpdate(List<Long> update, List<List<Long>> rules) {
        List<List<Long>> appRules = applicableRules(update, rules);
        for (List<Long> rule : appRules) {
            if (update.indexOf(rule.get(0)) >= update.indexOf(rule.get(1))) {
                return false;
            }
        }
        return true;
    }

    public static List<Long> middlePages(List<Long> update) {
        int len = update.size();
        int mid = len / 2;
        List<Long> result = new ArrayList<>();
        if (len % 2 == 1) {
            result.add(update.get(mid));
        } else {
            result.add(update.get(mid - 1));
            result.add(update.get(mid));
        }
        return result;
    }

    public static long sumMiddlePages(List<List<Long>> updates, List<List<Long>> rules) {
        long sum = 0;
        for (List<Long> update : updates) {
            if (validateUpdate(update, rules)) {
                List<Long> middle = middlePages(update);
                for (Long page : middle) {
                    sum += page;
                }
            }
        }
        return sum;
    }

    public static List<Long> correctUpdate(List<Long> update, List<List<Long>> rules) {
        List<Long> sortedUpdate = new ArrayList<>();
        List<Long> remainingUpdate = new ArrayList<>(update);

        while (!remainingUpdate.isEmpty()) {
            Long nextVal = remainingUpdate.remove(0);
            List<List<Long>> relevantRules = new ArrayList<>();
            for (List<Long> rule : rules) {
                if (rule.contains(nextVal)) {
                    relevantRules.add(rule);
                }
            }

            int correctIndex = 0;
            for (int i = 0; i <= sortedUpdate.size(); i++) {
                boolean validPlacement = true;
                for (List<Long> rule : relevantRules) {
                    List<Long> testUpdate = new ArrayList<>(sortedUpdate.subList(0, i));
                    testUpdate.add(nextVal);
                    testUpdate.addAll(sortedUpdate.subList(i, sortedUpdate.size()));

                    if (testUpdate.contains(rule.get(0)) && testUpdate.contains(rule.get(1))) {
                        if (testUpdate.indexOf(rule.get(0)) >= testUpdate.indexOf(rule.get(1))) {
                            validPlacement = false;
                            break;
                        }
                    }
                }
                if (validPlacement) {
                    correctIndex = i;
                    break;
                }
            }
            sortedUpdate.add(correctIndex, nextVal);
        }
        return sortedUpdate;
    }

    public static long sumMiddlePagesCorrected(List<List<Long>> updates, List<List<Long>> rules) {
        long sum = 0;
        for (List<Long> update : updates) {
            if (!validateUpdate(update, rules)) {
                List<Long> correctedUpdate = correctUpdate(update, rules);
                List<Long> middle = middlePages(correctedUpdate);
                for (Long page : middle) {
                    sum += page;
                }
            }
        }
        return sum;
    }

    public static long solve(String rulesStr, String updatesStr) {
        List<List<Long>> rules = parseRules(rulesStr);
        List<List<Long>> updates = parseUpdates(updatesStr);
        return sumMiddlePages(updates, rules);
    }

    public static long solvePart2(String rulesStr, String updatesStr) {
        List<List<Long>> rules = parseRules(rulesStr);
        List<List<Long>> updates = parseUpdates(updatesStr);
        return sumMiddlePagesCorrected(updates, rules);
    }
}
