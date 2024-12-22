def load_map(map_string):
    """Loads the map from a string."""
    return map_string.strip().split("\n")

def find_guard_start(map_data):
    """Finds the initial position of the guard."""
    for row, line in enumerate(map_data):
        for col, char in enumerate(line):
            if char == "^":
                return row, col
    return None, None

def move_guard(row, col, direction, map_data, visited):
    """Moves the guard based on the rules."""
    rows = len(map_data)
    cols = len(map_data[0])

    next_row, next_col = row, col
    if direction == "^":
        next_row -= 1
    elif direction == ">":
        next_col += 1
    elif direction == "v":
        next_row += 1
    elif direction == "<":
        next_col -= 1

    if 0 <= next_row < rows and 0 <= next_col < cols:
        if map_data[next_row][next_col] == "#":
            if direction == "^":
                direction = ">"
            elif direction == ">":
                direction = "v"
            elif direction == "v":
                direction = "<"
            elif direction == "<":
                direction = "^"
        else:
            row, col = next_row, next_col
            visited.add((row, col))
    else:
        return None, None, None # Guard is out of bounds

    return row, col, direction

def simulate_guard_movement(map_string):
    """Simulates the guard's movement and returns the number of visited positions."""
    map_data = load_map(map_string)
    start_row, start_col = find_guard_start(map_data)
    if start_row is None:
        return 0 # No guard found

    row, col = start_row, start_col
    direction = "^"
    visited = {(row, col)}

    while row is not None:
        row, col, direction = move_guard(row, col, direction, map_data, visited)

    return len(visited)

# Example usage:
map_string = """
..##..
#....#
#.^..#
#....#
"""
visited_count = simulate_guard_movement(map_string)
print(f"Number of distinct positions visited: {visited_count}")
