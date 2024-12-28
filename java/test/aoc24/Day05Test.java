package aoc24;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class Day05Test {

  private static final String EXAMPLE_RULES_STR = """
      47|53
      97|13
      97|61
      97|47
      75|29
      61|13
      75|53
      29|13
      97|29
      53|29
      61|53
      97|53
      61|29
      47|13
      75|47
      97|75
      47|61
      75|61
      47|29
      75|13
      53|13""";

  private static final String EXAMPLE_UPDATES_STR = """
      75,47,61,53,29
      97,61,53,29,13
      75,29,13
      75,97,47,61,53
      61,13,29
      97,13,75,29,47""";

  private static final List<List<Long>> EXAMPLE_RULES = Arrays.asList(
      Arrays.asList(47L, 53L),
      Arrays.asList(97L, 13L),
      Arrays.asList(97L, 61L),
      Arrays.asList(97L, 47L),
      Arrays.asList(75L, 29L),
      Arrays.asList(61L, 13L),
      Arrays.asList(75L, 53L),
      Arrays.asList(29L, 13L),
      Arrays.asList(97L, 29L),
      Arrays.asList(53L, 29L),
      Arrays.asList(61L, 53L),
      Arrays.asList(97L, 53L),
      Arrays.asList(61L, 29L),
      Arrays.asList(47L, 13L),
      Arrays.asList(75L, 47L),
      Arrays.asList(97L, 75L),
      Arrays.asList(47L, 61L),
      Arrays.asList(75L, 61L),
      Arrays.asList(47L, 29L),
      Arrays.asList(75L, 13L),
      Arrays.asList(53L, 13L));

  private static final List<List<Long>> EXAMPLE_UPDATES = Arrays.asList(
      Arrays.asList(75L, 47L, 61L, 53L, 29L),
      Arrays.asList(97L, 61L, 53L, 29L, 13L),
      Arrays.asList(75L, 29L, 13L),
      Arrays.asList(75L, 97L, 47L, 61L, 53L),
      Arrays.asList(61L, 13L, 29L),
      Arrays.asList(97L, 13L, 75L, 29L, 47L));

  @Test
  void testParseRules() {
    assertEquals(EXAMPLE_RULES, Day05.parseRules(EXAMPLE_RULES_STR));
  }

  @Test
  void testParseUpdates() {
    assertEquals(EXAMPLE_UPDATES, Day05.parseUpdates(EXAMPLE_UPDATES_STR));
  }

  @Test
  void testValidateUpdate() {
    assertTrue(Day05.validateUpdate(Arrays.asList(75L, 47L, 61L, 53L, 29L), EXAMPLE_RULES));
    assertTrue(Day05.validateUpdate(Arrays.asList(97L, 61L, 53L, 29L, 13L), EXAMPLE_RULES));
    assertTrue(Day05.validateUpdate(Arrays.asList(75L, 29L, 13L), EXAMPLE_RULES));
    assertFalse(Day05.validateUpdate(Arrays.asList(75L, 97L, 47L, 61L, 53L), EXAMPLE_RULES));
    assertFalse(Day05.validateUpdate(Arrays.asList(61L, 13L, 29L), EXAMPLE_RULES));
    assertFalse(Day05.validateUpdate(Arrays.asList(97L, 13L, 75L, 29L, 47L), EXAMPLE_RULES));
  }

  @Test
  void testMiddlePages() {
    assertEquals(Arrays.asList(61L), Day05.middlePages(Day05.parseUpdate("75,47,61,53,29")));
    assertEquals(Arrays.asList(53L), Day05.middlePages(Day05.parseUpdate("97,61,53,29,13")));
    assertEquals(Arrays.asList(29L), Day05.middlePages(Day05.parseUpdate("75,29,13")));
  }

  @Test
  void testSolve() {
    assertEquals(143L, Day05.solve(EXAMPLE_RULES_STR, EXAMPLE_UPDATES_STR));
  }

  @Test
  void testCorrectUpdate() {
    assertEquals(Arrays.asList(97L, 75L, 47L, 61L, 53L),
        Day05.correctUpdate(Arrays.asList(75L, 97L, 47L, 61L, 53L), EXAMPLE_RULES));
    assertEquals(Arrays.asList(61L, 29L, 13L), Day05.correctUpdate(Arrays.asList(61L, 13L, 29L), EXAMPLE_RULES));
    assertEquals(Arrays.asList(97L, 75L, 47L, 29L, 13L),
        Day05.correctUpdate(Arrays.asList(97L, 13L, 75L, 29L, 47L), EXAMPLE_RULES));
  }

  @Test
  void testSolvePart2() {
    assertEquals(123L, Day05.solvePart2(EXAMPLE_RULES_STR, EXAMPLE_UPDATES_STR));
  }
}
