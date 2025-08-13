def egg_count(encoded):
    """Count bits using Kernighan's Algorithm"""
    count = 0
    while encoded:
        encoded &= encoded - 1
        count += 1
    return count
