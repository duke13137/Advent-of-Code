def is_safe(board, row, col):
    """Check if placing a queen at (row, col) is valid"""
    # Check column
    for i in range(row):
        if board[i] == col:
            return False
        # Check diagonals
        if abs(board[i] - col) == row - i:
            return False
    return True

def solve_n_queens(n):
    """Main solver function that returns all valid solutions"""
    solutions = []
    
    def backtrack(row, current):
        if row == n:
            solutions.append(current.copy())
            return
        for col in range(n):
            if is_safe(current, row, col):
                current.append(col)
                backtrack(row + 1, current)
                current.pop()
    
    backtrack(0, [])
    return solutions

def print_solution(solution):
    """Pretty-print a single solution"""
    n = len(solution)
    for row in range(n):
        line = ["Q" if col == solution[row] else "." for col in range(n)]
        print(" ".join(line))
    print()

if __name__ == "__main__":
    n = 8  # Solve for 8-Queens problem
    solutions = solve_n_queens(n)
    
    print(f"Found {len(solutions)} solutions for {n}-Queens puzzle")
    if solutions:
        print("First solution:")
        print_solution(solutions[0])
