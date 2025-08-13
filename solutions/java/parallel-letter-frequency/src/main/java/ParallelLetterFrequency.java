import java.util.Map;
import java.util.List;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;

class ParallelLetterFrequency {

    private List<String> texts;
    private ConcurrentMap<Character, Integer> lettersCount;

    public ParallelLetterFrequency(String[] texts) {
        this.texts = List.of(texts);
        lettersCount = new ConcurrentHashMap<>();
    }

    public Map<Character, Integer> countLetters() {
        if (!lettersCount.isEmpty() || texts.isEmpty()) {
            return lettersCount;
        }
        texts.parallelStream().forEach(text -> {
            for (char c : text.toLowerCase().toCharArray()) {
                if (Character.isAlphabetic(c)) {
                    lettersCount.merge(c, 1, Integer::sum);
                }
            }
        });
        return lettersCount;
    }
}
