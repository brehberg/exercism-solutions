#include <array>
#include <string>
#include <vector>

#define FAILING_SCORE 40
#define PERFECT_SCORE 100

using namespace std;

// Round down all provided student scores.
vector<int> round_down_scores(vector<double> student_scores)
{
    vector<int> result = {};
    for (auto score : student_scores)
    {
        result.push_back((int)score);
    }
    return result;
}

// Count the number of failing students out of the group provided.
int count_failed_students(vector<int> student_scores)
{
    int failed_students{0};
    for (auto score : student_scores)
    {
        if (score <= FAILING_SCORE)
        {
            failed_students += 1;
        }
    }
    return failed_students;
}

// Determine how many of the provided student scores were 'the best' based on the provided threshold.
vector<int> above_threshold(vector<int> student_scores, int threshold)
{
    vector<int> the_best_scores = {};
    for (auto score : student_scores)
    {
        if (score >= threshold)
        {
            the_best_scores.emplace_back(score);
        }
    }
    return the_best_scores;
}

// Create a list of grade thresholds based on the provided highest grade.
array<int, 4> letter_grades(int highest_score)
{
    int step = (highest_score - FAILING_SCORE) / 4;
    return {FAILING_SCORE + 1,
            FAILING_SCORE + 1 + step,
            FAILING_SCORE + 1 + step * 2,
            FAILING_SCORE + 1 + step * 3};
}

// Organize the student's rank, name, and grade information in ascending order.
vector<string> student_ranking(vector<int> student_scores, vector<string> student_names)
{
    vector<string> ranking = {};
    for (size_t i = 0; i < student_scores.size(); i++)
    {
        string rank = to_string(i + 1);
        string name = student_names[i];
        string score = to_string(student_scores[i]);
        ranking.emplace_back(rank + ". " + name + ": " + score);
    }
    return ranking;
}

// Create a string that contains the name of the first student to make a perfect score on the exam.
string perfect_score(vector<int> student_scores, vector<string> student_names)
{
    for (size_t i = 0; i < student_scores.size(); i++)
    {
        if (student_scores[i] == PERFECT_SCORE)
        {
            return student_names[i];
        }
    }
    return "";
}
