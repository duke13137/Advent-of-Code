package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
        return Arrays.stream(rulesStr.split("\n"))
                .map(ruleStr -> {
                    String[] parts = ruleStr.split("\\|");
                    return Arrays.asList(Long.parseLong(parts[0]), Long.parseLong(parts[1]));
                })
                .collect(Collectors.toList());
    }

    public static List<Long> parseUpdate(String updateStr) {
        return Arrays.stream(updateStr.split(","))
                .map(Long::parseLong)
                .collect(Collectors.toList());
    }

    public static List<List<Long>> parseUpdates(String updatesStr) {
        return Arrays.stream(updatesStr.split("\n"))
                .map(Day05::parseUpdate)
                .collect(Collectors.toList());
    }


    public static List<List<Long>> applicableRules(List<Long> update, List<List<Long>> rules) {
        return rules.stream()
                .filter(rule -> update.contains(rule.get(0)) && update.contains(rule.get(1)))
                .collect(Collectors.toList());
    }

    public static boolean validateUpdate(List<Long> update, List<List<Long>> rules) {
        List<List<Long>> appRules = applicableRules(update, rules);
        return appRules.stream().allMatch(rule -> update.indexOf(rule.get(0)) < update.indexOf(rule.get(1)));
    }


    public static List<Long> middlePages(List<Long> update) {
        int len = update.size();
        int mid = len / 2;
        if (len % 2 == 1) {
            return Collections.singletonList(update.get(mid));
        } else {
            return Arrays.asList(update.get(mid - 1), update.get(mid));
        }
    }

    public static long sumMiddlePages(List<List<Long>> updates, List<List<Long>> rules) {
        return updates.stream()
                .filter(update -> validateUpdate(update, rules))
                .flatMap(update -> middlePages(update).stream())
                .reduce(0L, Long::sum);
    }

    public static List<Long> correctUpdate(List<Long> update, List<List<Long>> rules) {
        List<Long> sortedUpdate = new ArrayList<>();
        List<Long> remainingUpdate = new ArrayList<>(update);

        while (!remainingUpdate.isEmpty()) {
            Long nextVal = remainingUpdate.remove(0);
            List<List<Long>> relevantRules = rules.stream()
                    .filter(rule -> rule.contains(nextVal))
                    .collect(Collectors.toList());

            int correctIndex = 0;
            for (int i = 0; i <= sortedUpdate.size(); i++) {
                int finalI = i;
                boolean validPlacement = relevantRules.stream().allMatch(rule -> {
                    List<Long> testUpdate = new ArrayList<>(sortedUpdate.subList(0, finalI));
                    testUpdate.add(nextVal);
                    testUpdate.addAll(sortedUpdate.subList(finalI, sortedUpdate.size()));

                    if (testUpdate.contains(rule.get(0)) && testUpdate.contains(rule.get(1))) {
                        return testUpdate.indexOf(rule.get(0)) < testUpdate.indexOf(rule.get(1));
                    }
                    return true;
                });
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
        return updates.stream()
                .filter(update -> !validateUpdate(update, rules))
                .map(update -> correctUpdate(update, rules))
                .flatMap(correctedUpdate -> middlePages(correctedUpdate).stream())
                .reduce(0L, Long::sum);
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
