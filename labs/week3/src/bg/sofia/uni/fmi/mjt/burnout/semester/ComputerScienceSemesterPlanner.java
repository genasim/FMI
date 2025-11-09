package bg.sofia.uni.fmi.mjt.burnout.semester;

import bg.sofia.uni.fmi.mjt.burnout.exception.CryToStudentsDepartmentException;
import bg.sofia.uni.fmi.mjt.burnout.exception.InvalidSubjectRequirementsException;
import bg.sofia.uni.fmi.mjt.burnout.plan.SemesterPlan;
import bg.sofia.uni.fmi.mjt.burnout.subject.UniversitySubject;

public final class ComputerScienceSemesterPlanner extends AbstractSemesterPlanner {
    @Override
    protected void validateSemesterPlan(SemesterPlan plan) throws InvalidSubjectRequirementsException {
        validateCommon(plan);
    }

    @Override
    protected int compareSubjects(UniversitySubject a, UniversitySubject b) {
        int byRating = Integer.compare(b.rating(), a.rating());
        if (byRating != 0) return byRating;

        int byCredits = Integer.compare(b.credits(), a.credits());
        if (byCredits != 0) return byCredits;

        return Integer.compare(a.category().ordinal(), b.category().ordinal());
    }

    @Override
    public UniversitySubject[] calculateSubjectList(SemesterPlan semesterPlan)
        throws InvalidSubjectRequirementsException {

        validateSemesterPlan(semesterPlan);
        UniversitySubject[] subjects = semesterPlan.subjects();

        boolean[] chosenFlag = new boolean[subjects.length];
        int[] chosenIdx = new int[subjects.length];
        int chosenCount = 0;

        return buildResultSubjectsArray(semesterPlan, 0, chosenFlag, chosenIdx, chosenCount);
    }
}
