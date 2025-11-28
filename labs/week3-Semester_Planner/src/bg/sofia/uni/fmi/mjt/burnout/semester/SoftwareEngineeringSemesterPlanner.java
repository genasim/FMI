package bg.sofia.uni.fmi.mjt.burnout.semester;

import bg.sofia.uni.fmi.mjt.burnout.exception.CryToStudentsDepartmentException;
import bg.sofia.uni.fmi.mjt.burnout.exception.InvalidSubjectRequirementsException;
import bg.sofia.uni.fmi.mjt.burnout.plan.SemesterPlan;
import bg.sofia.uni.fmi.mjt.burnout.subject.SubjectRequirement;
import bg.sofia.uni.fmi.mjt.burnout.subject.UniversitySubject;

public final class SoftwareEngineeringSemesterPlanner extends AbstractSemesterPlanner {

    @Override
    protected void validateSemesterPlan(SemesterPlan plan) throws InvalidSubjectRequirementsException {
        validateCommon(plan);
        if (!hasEnoughSubjectsPerCategory(plan.subjects(), plan.subjectRequirements())) {
            throw new CryToStudentsDepartmentException(
                "There are to be enough subjects per category to cover enrollment requirements");
        }
    }

    @Override
    protected int compareSubjects(UniversitySubject a, UniversitySubject b) {
        int byCredits = Integer.compare(b.credits(), a.credits());
        if (byCredits != 0) return byCredits;

        return Integer.compare(a.category().ordinal(), b.category().ordinal());
    }

    @Override
    public UniversitySubject[] calculateSubjectList(SemesterPlan semesterPlan)
        throws InvalidSubjectRequirementsException {

        validateSemesterPlan(semesterPlan);

        UniversitySubject[] subjects = semesterPlan.subjects();
        SubjectRequirement[] requirements = semesterPlan.subjectRequirements();

        boolean[] chosenFlag = new boolean[subjects.length];
        int[] chosenIdx = new int[subjects.length];
        int chosenCount = 0;

        for (SubjectRequirement req : requirements) {
            int k = req.minAmountEnrolled();
            if (k <= 0) continue;

            int[] catIdx = new int[subjects.length];
            int catCount = 0;
            for (int i = 0; i < subjects.length; i++) {
                UniversitySubject s = subjects[i];
                if (s != null && s.category() == req.category()) {
                    catIdx[catCount++] = i;
                }
            }
            insertionSortIndices(catIdx, catCount, subjects);

            for (int t = 0; t < k; t++) {
                int idx = catIdx[t];
                if (!chosenFlag[idx]) {
                    chosenFlag[idx] = true;
                    chosenIdx[chosenCount++] = idx;
                }
            }
        }

        return buildResultSubjectsArray(semesterPlan, sumCredits(chosenIdx, chosenCount, subjects), chosenFlag,
                                        chosenIdx, chosenCount);
    }

    private boolean hasEnoughSubjectsPerCategory(UniversitySubject[] subjects, SubjectRequirement[] requirements) {
        for (SubjectRequirement requirement : requirements) {
            if (requirement == null) continue;
            int count = 0;
            for (UniversitySubject subject : subjects) {
                if (subject.category() == requirement.category()) {
                    count++;
                }
            }
            if (count < requirement.minAmountEnrolled()) {
                return false;
            }
        }
        return true;
    }

    private int sumCredits(int[] idx, int count, UniversitySubject[] subjects) {
        int sum = 0;
        for (int i = 0; i < count; i++) {
            sum += subjects[idx[i]].credits();
        }
        return sum;
    }
}
