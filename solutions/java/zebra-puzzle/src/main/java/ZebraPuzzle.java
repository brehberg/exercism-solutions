import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Solve the Zebra puzzle using the AC-3 algorithm.
 *
 * <p>
 * In AC-3 terms: - The variables are the house attributes: pets, nationalities,
 * color, drink and hobby. Their domains are the house numbers (1, 2, 3, 4 & 5).
 */
class ZebraPuzzle {

    private static final Collection<Integer> DOMAIN = Arrays.asList(1, 2, 3, 4, 5);

    enum VariableType {
        COLOR,
        HOBBY,
        DRINK,
        NATION,
        PET
    }

    /** Represents the AC-3 variables. */
    enum Variable {
        // Colors
        RED(VariableType.COLOR),
        GREEN(VariableType.COLOR),
        IVORY(VariableType.COLOR),
        YELLOW(VariableType.COLOR),
        BLUE(VariableType.COLOR),

        // Hobbies
        PAINTING(VariableType.HOBBY),
        DANCING(VariableType.HOBBY),
        READING(VariableType.HOBBY),
        FOOTBALL(VariableType.HOBBY),
        CHESS(VariableType.HOBBY),

        // Drinks
        COFFEE(VariableType.DRINK),
        TEA(VariableType.DRINK),
        MILK(VariableType.DRINK),
        ORANGE_JUICE(VariableType.DRINK),
        WATER(VariableType.DRINK),

        // Nationalities
        ENGLISHMAN(VariableType.NATION, "Englishman"),
        SPANIARD(VariableType.NATION, "Spaniard"),
        UKRAINIAN(VariableType.NATION, "Ukrainian"),
        NORWEGIAN(VariableType.NATION, "Norwegian"),
        JAPANESE(VariableType.NATION, "Japanese"),

        // Pets
        DOG(VariableType.PET),
        FOX(VariableType.PET),
        HORSE(VariableType.PET),
        ZEBRA(VariableType.PET),
        SNAILS(VariableType.PET);

        final VariableType type;

        final String text;

        Variable(VariableType type) {
            this(type, "");
        }

        Variable(VariableType type, String text) {
            this.type = type;
            this.text = text;
        }
    }

    static final Map<VariableType, List<Variable>> variablesByType = Stream.of(Variable.values())
            .collect(Collectors.groupingBy(variable -> variable.type));

    /**
     * Representation of an arc in AC-3. Instances of these should be derived from
     * the constraints,
     * such as the rules.
     */
    static class Arc {
        private final Variable leftVariable;

        /**
         * the variable(s) on the "right hand side". values in the domain on the left
         * hand side may be
         * removed depending on the values in domain of these variable. may be empty if
         * it doesn't
         * depend on any other variables (for example, in rules like "the Norwegian
         * lives in the first
         * house") or may depend on multiple other variables (for example, if a value
         * appears in only
         * one of the variable's domain)
         */
        private final Collection<Variable> rightVariables;

        private final Checker checker;

        Arc(Variable leftVariable, Checker check) {
            this(leftVariable, Collections.emptyList(), check);
        }

        Arc(Variable leftVariable, Variable rightVariable, Checker checker) {
            this(leftVariable, Collections.singleton(rightVariable), checker);
        }

        Arc(Variable leftVariable, Collection<Variable> rightVariables, Checker checker) {
            this.leftVariable = leftVariable;
            this.rightVariables = rightVariables;
            this.checker = checker;
        }

        boolean check(Map<Variable, Collection<Integer>> possibilities) {
            Collection<Integer> leftDomain = possibilities.get(leftVariable);
            Collection<Integer> combinedRight = rightVariables.stream()
                    .flatMap(v -> possibilities.get(v).stream())
                    .collect(Collectors.toSet());
            return checker.check(leftDomain, combinedRight);
        }
    }

    /**
     * Used by {@link Arc} to provide a generic way of removing inconsistent values
     * from a domian.
     */
    @FunctionalInterface
    interface Checker {

        /**
         * @param leftDomain   the domain of possible values on the left hand side.
         *                     implementations should
         *                     remove values from this {@link Collection} if they no
         *                     apply.
         * @param rightDomains the set of all values in the right domain. implementation
         *                     shouldn't remove
         *                     values from this {@link Collection}.
         * @return {@code true} if values in the left domain was changed, otherwise
         *         {@code false}.
         */
        boolean check(Collection<Integer> leftDomain, Collection<Integer> rightDomains);
    }

    /**
     * Makes the arcs for the binary constraints.
     *
     * @return the arcs for the binary constaints
     */
    static Collection<Arc> arcs() {
        List<Arc> arcs = new ArrayList<>(
                List.of(
                        // 2. The Englishman lives in the red house.
                        sameHouse(Variable.ENGLISHMAN, Variable.RED),
                        sameHouse(Variable.RED, Variable.ENGLISHMAN),

                        // 3. The Spaniard owns the dog.
                        sameHouse(Variable.SPANIARD, Variable.DOG),
                        sameHouse(Variable.DOG, Variable.SPANIARD),

                        // 4. The person in the green house drinks coffee.
                        sameHouse(Variable.COFFEE, Variable.GREEN),
                        sameHouse(Variable.GREEN, Variable.COFFEE),

                        // 5. The Ukrainian drinks tea.
                        sameHouse(Variable.UKRAINIAN, Variable.TEA),
                        sameHouse(Variable.TEA, Variable.UKRAINIAN),

                        // 6. The green house is immediately to the right of the ivory house.
                        nextTo(Variable.GREEN, Variable.IVORY, +1),
                        nextTo(Variable.IVORY, Variable.GREEN, -1),

                        // 7. The snail owner likes to go dancing.
                        sameHouse(Variable.DANCING, Variable.SNAILS),
                        sameHouse(Variable.SNAILS, Variable.DANCING),

                        // 8. The person in the yellow house is a painter.
                        sameHouse(Variable.PAINTING, Variable.YELLOW),
                        sameHouse(Variable.YELLOW, Variable.PAINTING),

                        // 9. The person in the middle house drinks milk.
                        knownHouse(Variable.MILK, 3),

                        // 10. The Norwegian lives in the first house.
                        knownHouse(Variable.NORWEGIAN, 1),

                        // 11. The person who enjoys reading lives in the house next to the person with
                        // the fox.
                        nextTo(Variable.READING, Variable.FOX),
                        nextTo(Variable.FOX, Variable.READING),

                        // 12. The painter's house is next to the house with the horse.
                        nextTo(Variable.PAINTING, Variable.HORSE),
                        nextTo(Variable.HORSE, Variable.PAINTING),

                        // 13. The person who plays football drinks orange juice.
                        sameHouse(Variable.FOOTBALL, Variable.ORANGE_JUICE),
                        sameHouse(Variable.ORANGE_JUICE, Variable.FOOTBALL),

                        // 14. The Japanese person plays chess.
                        sameHouse(Variable.JAPANESE, Variable.CHESS),
                        sameHouse(Variable.CHESS, Variable.JAPANESE),

                        // 15. The Norwegian lives next to the blue house.
                        nextTo(Variable.NORWEGIAN, Variable.BLUE),
                        nextTo(Variable.BLUE, Variable.NORWEGIAN)));

        // Additional arcs

        // 6. The green house is immediately to the right of the ivory house.
        // If we narrow down the range of possible values of the green and ivory house
        // to exactly three
        // consecutive numbers, then we know that the middle option MUST be either green
        // or ivory to fit
        // this constraint. In other words, we can remove the middle option from the
        // other colours.
        List<Variable> adjacentVariables = List.of(Variable.GREEN, Variable.IVORY);
        for (Variable variable : variablesByType.get(VariableType.COLOR)) {
            if (!adjacentVariables.contains(variable)) {
                arcs.add(notInBetween(variable, adjacentVariables));
            }
        }

        for (List<Variable> variableGroup : variablesByType.values()) {
            for (Variable left : variableGroup) {
                List<Variable> otherVariables = new ArrayList<>(variableGroup);
                otherVariables.remove(left);

                // Arcs to represent mutually exclusive variables. For example, if a house is
                // GREEN, it
                // can't be RED, BLUE, YELLOW or IVORY.
                otherVariables.stream().map(right -> mutuallyExclusive(left, right)).forEach(arcs::add);

                // Special arc that checks each group of value type. If the house can be in only
                // one
                // position, then mark that variable as the house. For example, lets say we get
                // to
                // the following state for the colors:
                // RED: [3, 4, 5]
                // GREEN: [4, 5]
                // IVORY: [3, 4]
                // YELLOW: [1, 3, 4, 5]
                // BLUE: [2]
                // Since 1 appears only in yellow, house 1 MUST be YELLOW.
                arcs.add(onlyPossibleHouse(left, otherVariables));
            }
        }

        return arcs;
    }

    // Utilities for making the AC3 arcs.

    /**
     * Variables must be in the same house.
     *
     * @param leftVariable
     * @param rightVariable
     * @return
     */
    static Arc sameHouse(Variable leftVariable, Variable rightVariable) {
        return new Arc(
                leftVariable,
                rightVariable,
                (leftDomain, rightDomain) -> leftDomain.retainAll(rightDomain));
    }

    /**
     * The two variables are next to each other.
     *
     * @param leftVariable
     * @param rightVariable
     * @return
     */
    static Arc nextTo(Variable leftVariable, Variable rightVariable) {
        return new Arc(
                leftVariable,
                rightVariable,
                (leftDomain, rightDomain) -> leftDomain.removeIf(
                        value -> !rightDomain.contains(value - 1) && !rightDomain.contains(value + 1)));
    }

    /**
     * Left variable's house number is offest by a specific amount to the right
     * variable.
     *
     * @param leftVariable
     * @param rightVariable
     * @param offset        set to +1 if the left variable is just to the right of
     *                      the right variable or -1
     *                      if it is to left.
     */
    static Arc nextTo(Variable leftVariable, Variable rightVariable, int offset) {
        return new Arc(
                leftVariable,
                rightVariable,
                (leftDomain, rightDomain) -> leftDomain.removeIf(value -> !rightDomain.contains(value - offset)));
    }

    /**
     * Provides an arc for a known house value.
     *
     * @param variable
     * @param expectedHouse
     * @return {@code true} if the domain was changed, otherwise {@code false}.
     */
    static Arc knownHouse(Variable variable, int expectedHouse) {
        return new Arc(variable, (domain, otherDomain) -> domain.retainAll(Collections.singleton(expectedHouse)));
    }

    /**
     * Left variable and right variable can't be the same. This arc modifies the
     * left's domain only
     * when the right hand's domain has exactly one value because it can only be
     * sure the value can be
     * removed from the left hand's domain when the right hand's value is certain.
     *
     * @param leftVariable
     * @param rightVariable
     * @return
     */
    static Arc mutuallyExclusive(Variable leftVariable, Variable rightVariable) {
        return new Arc(
                leftVariable,
                rightVariable,
                (leftDomain, rightDomain) -> {
                    if (rightDomain.size() == 1) {
                        return leftDomain.removeAll(rightDomain);
                    }
                    return false;
                });
    }

    /**
     * Checks the domain of the left variable to see if any of its values is the
     * only possible place
     * where the house may occur. If it is, removes the other values from its
     * domain. For example,
     * lets say we get to the following state for the colours:
     *
     * <pre>
     *   RED: [3, 4, 5]
     *   GREEN: [4, 5]
     *   IVORY: [3, 4]
     *   YELLOW: [1, 3, 4, 5]
     *   BLUE: [2]
     * </pre>
     *
     * <p>
     * Since 1 appears only in YELLOW's domain, house 1 MUST be YELLOW.
     *
     * <p>
     * Note, this does not remove any values if the variable's domain has only 1
     * value left. For
     * example, this arc will do nothing for BLUE.
     *
     * @param leftVariable
     * @param rightVariables
     * @return
     */
    static Arc onlyPossibleHouse(Variable leftVariable, Collection<Variable> rightVariables) {
        return new Arc(
                leftVariable,
                rightVariables,
                (leftDomain, combinedRight) -> {
                    if (leftDomain.size() < 2) {
                        return false;
                    }

                    Optional<Integer> uniqueValue = leftDomain.stream().filter(value -> !combinedRight.contains(value))
                            .findFirst();
                    if (uniqueValue.isPresent()) {
                        return leftDomain.retainAll(Collections.singleton(uniqueValue.get()));
                    }

                    return false;
                });
    }

    /**
     * This arc is to support further reductions from known adjacent variables. For
     * example, consider
     * the following rule:
     *
     * <p>
     * 6. The green house is immediately to the right of the ivory house.
     *
     * <p>
     * If we narrow down the range of possible values of the green and ivory house
     * to exactly three
     * consecutive numbers, then we know that the middle option MUST be either green
     * or ivory to fit
     * this constraint. In other words, we can remove the middle option from the
     * other colours.
     *
     * @param leftVariable
     * @param adjacentVariables
     * @return
     */
    static Arc notInBetween(Variable leftVariable, Collection<Variable> adjacentVariables) {
        return new Arc(
                leftVariable,
                adjacentVariables,
                (leftDomain, combinedRight) -> {
                    if (combinedRight.size() == 3) {
                        OptionalInt min = combinedRight.stream().mapToInt(Integer::intValue).min();
                        OptionalInt max = combinedRight.stream().mapToInt(Integer::intValue).max();
                        if (min.isPresent() && max.isPresent() && max.getAsInt() == min.getAsInt() + 2) {
                            return leftDomain.remove(min.getAsInt() + 1);
                        }
                    }
                    return false;
                });
    }

    private final Map<Variable, Integer> houseByVariable = new HashMap<>();

    private final Map<Integer, Collection<Variable>> variablesByHouse = new HashMap<>();

    ZebraPuzzle() {
        // Initialise a map of all possible values.
        Map<Variable, Collection<Integer>> possibleValues = new HashMap<>();
        for (Variable variable : Variable.values()) {
            possibleValues.put(variable, new HashSet<>(DOMAIN));
        }

        Collection<Arc> arcs = arcs();
        reduce(possibleValues, arcs);

        boolean solved = isSolved(possibleValues);
        if (!solved) {
            // At this point, we have deduced all we can based on the contraints. So here,
            // we'll test some
            // values to eliminate some possibilitise.
            Iterator<Variable> variableIterator = getUnsolvedVariables(possibleValues).iterator();
            while (!solved && variableIterator.hasNext()) {
                Variable testVariable = variableIterator.next();
                List<Integer> tryValues = List.copyOf(possibleValues.get(testVariable));

                if (tryValues.size() > 1) {
                    Iterator<Integer> valueIterator = tryValues.iterator();
                    while (!solved && valueIterator.hasNext()) {
                        Integer testValue = valueIterator.next();

                        // Make a copy to allow testing. We don't necessarily want to add it to our kept
                        // list
                        // yet.
                        Map<Variable, Collection<Integer>> testCopy = makeCopy(possibleValues);
                        Collection<Arc> testArcs = new ArrayList<>(arcs);
                        testArcs.add(knownHouse(testVariable, testValue));

                        try {
                            reduce(testCopy, testArcs);

                            // In case we happen to have solve the puzzle.
                            if (isSolved(testCopy)) {
                                solved = true;
                                possibleValues = testCopy;
                            }
                        } catch (IllegalStateException e) {
                            // This means the house can NOT be the tested variable and we can remove the
                            // house
                            // from the variable's domain.
                            possibleValues.get(testVariable).remove(testValue);
                            reduce(possibleValues, arcs);
                            solved = isSolved(possibleValues);
                        }
                    }
                }
            }
        }

        if (!solved) {
            // If we still haven't managed to solve it, something has gone wrong.
            throw new IllegalStateException("Puzzle is still not solved!");
        }

        possibleValues.forEach(
                (variable, domain) -> {
                    if (domain.size() != 1) {
                        throw new IllegalStateException(
                                variable + "'s domain has " + domain.size() + " elements, expecting 1");
                    }

                    int house = domain.iterator().next();
                    houseByVariable.put(variable, house);
                    variablesByHouse
                            .computeIfAbsent(house, h -> EnumSet.noneOf(Variable.class))
                            .add(variable);
                });
    }

    /**
     * Determines whether the puzzle has been solved. The puzzle is considered
     * solved when all
     * variables have exactly one value in their domain.
     *
     * @param domainByVariable map from the variables to their domain of possible
     *                         values.
     * @return {@code true} if the puzzle has been solved, otherwise {@code false}.
     */
    static boolean isSolved(Map<Variable, Collection<Integer>> domainByVariable) {
        return domainByVariable.values().stream().allMatch(values -> values.size() == 1);
    }

    /**
     * Extracts all the unsolved variables. A variable is unsolved if it still has
     * more than one value
     * in its domain.
     *
     * @param domainByVariable map from the variables to their domains.
     * @return the {@link Collection} of unsolved variables.
     */
    static Collection<Variable> getUnsolvedVariables(
            Map<Variable, Collection<Integer>> domainByVariable) {
        return Stream.of(Variable.values())
                .filter(key -> domainByVariable.get(key).size() > 1)
                .toList();
    }

    /**
     * Runs the AC-3 algorithm to remove values from the domains that are
     * inconsistent with any of the
     * arcs.
     *
     * @param domainByVariable map from the variables to their domains. note, this
     *                         map will be edited
     *                         directly!
     * @param arcs             provides the constraints on the domains. values will
     *                         be removed from the domains
     *                         according to these arcs.
     * @throws IllegalStateException if the reduction results in one of the domains
     *                               becoming empty.
     */
    static void reduce(Map<Variable, Collection<Integer>> domainByVariable, Collection<Arc> arcs)
            throws IllegalStateException {
        // Map the variables to the arcs interested in it. This will make it easier to
        // figure out
        // which arcs need to re-enqueued later.
        Map<Variable, List<Arc>> arcInterests = new HashMap<>();
        arcs.forEach(
                arc -> {
                    for (Variable rightVariable : arc.rightVariables) {
                        arcInterests.computeIfAbsent(rightVariable, x -> new ArrayList<>()).add(arc);
                    }
                });

        // Go through the arcs and remove values from the domains that are inconsistent.
        ArrayDeque<Arc> workQueue = new ArrayDeque<>(arcs);
        while (!workQueue.isEmpty()) {
            Arc arc = workQueue.poll();
            if (arc.check(domainByVariable)) {
                if (domainByVariable.get(arc.leftVariable).isEmpty()) {
                    throw new IllegalStateException("A domain is empty!");
                }

                // If a change was made, ensure that any arcs that are interested in the changed
                // domain are
                // enqueued. Keep in mind, the arc may also still be coming up in the queue.
                List<Arc> toEnqueue = arcInterests.getOrDefault(arc.leftVariable, Collections.emptyList());
                for (Arc requiredArc : toEnqueue) {
                    if (!workQueue.contains(requiredArc)) {
                        workQueue.add(requiredArc);
                    }
                }
            }
        }
    }

    static Map<Variable, Collection<Integer>> makeCopy(Map<Variable, Collection<Integer>> from) {
        Map<Variable, Collection<Integer>> copy = new HashMap<>();
        from.forEach((variable, domain) -> copy.put(variable, new ArrayList<>(domain)));
        return copy;
    }

    String getWaterDrinker() {
        return getWho(Variable.WATER);
    }

    String getZebraOwner() {
        return getWho(Variable.ZEBRA);
    }

    private String getWho(Variable variable) {
        int house = houseByVariable.get(variable);
        for (Variable v : variablesByHouse.get(house)) {
            if (v.type == VariableType.NATION) {
                return v.text;
            }
        }
        throw new IllegalStateException("Can't work out who has " + variable);
    }
}