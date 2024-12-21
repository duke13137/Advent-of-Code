package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Day04 {

  public static void main(String[] args) throws IOException {
    Path inputPath = Path.of("resources", "aoc24", "day04.txt");
    List<String> input = Files.readAllLines(inputPath);

    long part1Result = countOccurrences(input);
    System.out.println("Part 1: " + part1Result);

    long part2Result = countXMASShapes(input);
    System.out.println("Part 2: " + part2Result);
  }

  public static long countOccurrences(List<String> grid) {
    int rows = grid.size();
    int cols = grid.get(0).length();
    String word = "XMAS";
    int wordLen = word.length();
    long count = 0;

    for (int row = 0; row < rows; row++) {
      for (int col = 0; col < cols; col++) {
        count += checkDirections(grid, row, col, word, wordLen);
      }
    }

    return count;
  }

  private static int checkDirections(List<String> grid, int row, int col, String word, int wordLen) {
    int count = 0;
    int[][] directions = { { 0, 1 }, { 1, 0 }, { 1, 1 }, { 1, -1 } }; // right, down, diag-down-right, diag-down-left

    for (int[] dir : directions) {
      int dirRow = dir[0];
      int dirCol = dir[1];
      String forward = extractString(grid, row, col, wordLen, dirRow, dirCol);
      String backward = new StringBuilder(forward).reverse().toString();
      if (forward.equals(word))
        count++;
      if (backward.equals(word))
        count++;
    }
    return count;
  }

  private static String extractString(List<String> grid, int row, int col, int wordLen, int dirRow, int dirCol) {
    StringBuilder sb = new StringBuilder();
    int rows = grid.size();
    int cols = grid.get(0).length();

    for (int i = 0; i < wordLen; i++) {
      int newRow = row + i * dirRow;
      int newCol = col + i * dirCol;

      if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
        sb.append(grid.get(newRow).charAt(newCol));
      } else {
        return "";
      }
    }
    return sb.toString();
  }

  public static long countXMASShapes(List<String> grid) {
    int rows = grid.size();
    int cols = grid.get(0).length();
    long count = 0;

    for (int row = 1; row < rows - 1; row++) {
      for (int col = 1; col < cols - 1; col++) {
        if (isXMASShape(grid, row, col)) {
          count++;
        }
      }
    }

    return count;
  }

  private static boolean isXMASShape(List<String> grid, int row, int col) {
    int[] mas1 = checkDiagonalForMAS(grid, row, col, 1, 1);
    int[] mas2 = checkOppositeDiagonalForMAS(grid, row, col, -1, 1);

    if (mas1 != null && mas2 != null) {
      // Check if the two MAS sequences intersect at a common point
      return mas1[1] == mas2[1] && mas1[4] == mas2[4];
    }

    return false;
  }

  private static int[] checkDiagonalForMAS(List<String> grid, int row, int col, int dirRow, int dirCol) {
    String word = "MAS";
    int wordLen = word.length();
    int rows = grid.size();
    int cols = grid.get(0).length();

    for (int i = -wordLen + 1; i < wordLen; i++) {
      int startRow = row - dirRow + i * dirRow;
      int startCol = col - dirCol + i * dirCol;

      if (startRow >= 0 && startRow < rows - wordLen + 1 && startCol >= 0 && startCol < cols - wordLen + 1) {
        String forward = extractString(grid, startRow, startCol, wordLen, dirRow, dirCol);
        if (forward.equals(word) || new StringBuilder(forward).reverse().toString().equals(word)) {
          return new int[] { startRow, startCol, startRow + dirRow, startCol + dirCol,
              startRow + 2 * dirRow, startCol + 2 * dirCol };
        }
      }
    }

    return null;
  }

  private static int[] checkOppositeDiagonalForMAS(List<String> grid, int row, int col, int dirRow, int dirCol) {
    return checkDiagonalForMAS(grid, row, col, dirRow, dirCol);
  }
}
