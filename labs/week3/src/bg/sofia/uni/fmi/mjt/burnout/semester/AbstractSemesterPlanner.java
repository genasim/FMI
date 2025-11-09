package bg.sofia.uni.fmi.mjt.burnout.semester;

import bg.sofia.uni.fmi.mjt.burnout.exception.CryToStudentsDepartmentException;
import bg.sofia.uni.fmi.mjt.burnout.exception.DisappointmentException;
import bg.sofia.uni.fmi.mjt.burnout.exception.InvalidSubjectRequirementsException;
import bg.sofia.uni.fmi.mjt.burnout.plan.SemesterPlan;
import bg.sofia.uni.fmi.mjt.burnout.subject.Category;
import bg.sofia.uni.fmi.mjt.burnout.subject.SubjectRequirement;
import bg.sofia.uni.fmi.mjt.burnout.subject.UniversitySubject;

public abstract sealed class AbstractSemesterPlanner implements SemesterPlannerAPI
    permits SoftwareEngineeringSemesterPlanner, ComputerScienceSemesterPlanner {

    protected abstract int compareSubjects(UniversitySubject a, UniversitySubject b);

    protected abstract void validateSemesterPlan(SemesterPlan plan) throws InvalidSubjectRequirementsException;

    @Override
    public int calculateJarCount(UniversitySubject[] subjects, int maximumSlackTime, int semesterDuration) {
        if (maximumSlackTime <= 0 || semesterDuration <= 0) {
            throw new IllegalArgumentException("slack and semester duration times must be positive");
        }

        if (subjects == null || subjects.length == 0) {
            throw new IllegalArgumentException("subjects must not be null or empty");
        }

        int totalRestTime = 0;
        int totalStudyTime = 0;
        for (UniversitySubject subject : subjects) {
            totalRestTime += (int) Math.ceil(subject.neededStudyTime() * subject.category().getCoefficient());
            totalStudyTime += subject.neededStudyTime();
        }

        if (totalRestTime > maximumSlackTime) {
            throw new DisappointmentException("Slack time exceeds maximum slack time - grandma is very disappointed");
        }

        int jars = (int) Math.ceil((double) totalStudyTime / 5);

        int totalWorkTime = totalRestTime + totalStudyTime;
        return totalWorkTime > semesterDuration ? jars * 2 : jars;
    }

    protected final Category findDuplicateCategory(SubjectRequirement[] requirements) {
        boolean[] seen = new boolean[Category.values().length];
        for (SubjectRequirement r : requirements) {
            if (r == null) continue;
            int ord = r.category().ordinal();
            if (seen[ord]) return r.category();
            seen[ord] = true;
        }
        return null;
    }

    protected final UniversitySubject[] buildResultSubjectsArray(
        SemesterPlan plan, int currentCredits, boolean[] flags, int[] indices, int count) {
        while (currentCredits < plan.minimalAmountOfCredits()) {
            int bestIdx = pickBestRemainingIndex(plan.subjects(), flags);
            if (bestIdx == -1) break;
            flags[bestIdx] = true;
            indices[count++] = bestIdx;
            currentCredits += plan.subjects()[bestIdx].credits();
        }
        if (currentCredits < plan.minimalAmountOfCredits()) {
            throw new CryToStudentsDepartmentException(
                "Cannot reach minimal credits " + plan.minimalAmountOfCredits() + " with available plan");
        }

        insertionSortIndices(indices, count, plan.subjects());

        UniversitySubject[] result = new UniversitySubject[count];
        for (int i = 0; i < count; i++) {
            result[i] = plan.subjects()[indices[i]];
        }
        return result;
    }

    protected final void validateCommon(SemesterPlan plan) throws InvalidSubjectRequirementsException {
        if (plan == null) throw new IllegalArgumentException("Semester plan is null");

        Category dup = findDuplicateCategory(plan.subjectRequirements());
        if (dup != null) {
            throw new InvalidSubjectRequirementsException("Duplicate category: " + dup.name());
        }
    }


    protected final void insertionSortIndices(int[] idx, int count, UniversitySubject[] subjects) {
        for (int i = 1; i < count; i++) {
            int key = idx[i];
            int j = i - 1;
            while (j >= 0 && compareSubjects(subjects[key], subjects[idx[j]]) < 0) {
                idx[j + 1] = idx[j];
                j--;
            }
            idx[j + 1] = key;
        }
    }

    private int pickBestRemainingIndex(UniversitySubject[] subjects, boolean[] chosenFlag) {
        int best = -1;
        for (int i = 0; i < subjects.length; i++) {
            if (subjects[i] == null || chosenFlag[i]) continue;
            if (best == -1 || compareSubjects(subjects[i], subjects[best]) < 0) best = i;
        }
        return best;
    }
}
