class SqueakyClean {
    static String clean(String identifier) {
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < identifier.length(); i++) {
            char c = identifier.charAt(i);

            if (Character.isSpaceChar(c)) {
                // Replace any spaces encountered with underscores
                result.append('_');

            } else if (c == '-') {
                // Convert kebab-case to camelCase
                i += 1;
                result.append(Character.toUpperCase(identifier.charAt(i)));

            } else if (Character.isDigit(c)) {
                // Convert leetspeak to normal text
                switch (c) {
                    case '4':
                        result.append('a');
                        break;
                    case '3':
                        result.append('e');
                        break;
                    case '0':
                        result.append('o');
                        break;
                    case '1':
                        result.append('l');
                        break;
                    case '7':
                        result.append('t');
                        break;
                    default:
                        result.append(c);
                        break;
                }
            } else if (Character.isLetter(c)) {
                // Omit characters that are not letters
                result.append(c);
            }
        }

        return result.toString();
    }
}
