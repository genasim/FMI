package bg.sofia.uni.fmi.mjt.burnout;

import bg.sofia.uni.fmi.mjt.burnout.exception.CryToStudentsDepartmentException;
import bg.sofia.uni.fmi.mjt.burnout.exception.InvalidSubjectRequirementsException;
import bg.sofia.uni.fmi.mjt.burnout.plan.SemesterPlan;
import bg.sofia.uni.fmi.mjt.burnout.semester.SemesterPlannerAPI;
import bg.sofia.uni.fmi.mjt.burnout.semester.SoftwareEngineeringSemesterPlanner;
import bg.sofia.uni.fmi.mjt.burnout.subject.Category;
import bg.sofia.uni.fmi.mjt.burnout.subject.SubjectRequirement;
import bg.sofia.uni.fmi.mjt.burnout.subject.UniversitySubject;

import java.util.Arrays;


public class Main {

    public static void main(String... args) throws InvalidSubjectRequirementsException {
        {
            SoftwareEngineeringSemesterPlanner planner = new SoftwareEngineeringSemesterPlanner();

            UniversitySubject[] subjects = {
                new UniversitySubject("Calculus", 6, 5, Category.MATH, 40),
                new UniversitySubject("Java Programming", 4, 4, Category.PROGRAMMING, 30),
                new UniversitySubject("Linear Algebra", 5, 3, Category.MATH, 35),
                new UniversitySubject("Data Structures", 3, 5, Category.PROGRAMMING, 25)
            };

            SubjectRequirement[] requirements = {
                new SubjectRequirement(Category.MATH, 1),
                new SubjectRequirement(Category.PROGRAMMING, 1)
            };

            SemesterPlan plan1 = new SemesterPlan(subjects, requirements, 5);

            printSubjects(planner.calculateSubjectList(plan1));
            //result1 = ["Calculus", "Java Programming"]

            SemesterPlan plan2 = new SemesterPlan(subjects, requirements, 10);

            //result2 = ["Calculus", "Java Programming"]
            printSubjects(planner.calculateSubjectList(plan2));

            SemesterPlan plan3 = new SemesterPlan(subjects, requirements, 15);

            //result3 = ["Calculus", "Linear Algebra", "Java Programming"]
            printSubjects(planner.calculateSubjectList(plan3));

            UniversitySubject[] selectedSubjects = planner.calculateSubjectList(plan1);

            int jarCount = planner.calculateJarCount(selectedSubjects, 11, 50);
            System.out.println("Jar count: " + jarCount);
            //jarCount = 28
        }
        System.out.println();
        {
            SemesterPlannerAPI planner = new SoftwareEngineeringSemesterPlanner();

            UniversitySubject[] subjects = {
                new UniversitySubject("Calculus", 6, 5, Category.MATH, 40),
                new UniversitySubject("Java Programming", 4, 4, Category.PROGRAMMING, 30),
                new UniversitySubject("Linear Algebra", 5, 3, Category.MATH, 35),
                new UniversitySubject("Data Structures", 3, 5, Category.PROGRAMMING, 25),
                new UniversitySubject("Discrete Math", 4, 5, Category.THEORY, 20)
            };

            SubjectRequirement[] req1 = {
                new SubjectRequirement(Category.MATH, 1),
                new SubjectRequirement(Category.PROGRAMMING, 1)
            };
            SemesterPlan plan1 = new SemesterPlan(subjects, req1, 4);
            printSubjects(planner.calculateSubjectList(plan1));
            // output: [Calculus(6,MATH), Java Programming(4,PROGRAMMING)]

            SemesterPlan plan2 = new SemesterPlan(subjects, req1, 10);
            printSubjects(planner.calculateSubjectList(plan2));
            // output: [Calculus(6,MATH), Java Programming(4,PROGRAMMING)]

            SemesterPlan plan3 = new SemesterPlan(subjects, req1, 15);
            printSubjects(planner.calculateSubjectList(plan3));
            // output: [Calculus(6,MATH), Linear Algebra(5,MATH), Java Programming(4,PROGRAMMING)]

            SubjectRequirement[] req2 = {
                new SubjectRequirement(Category.MATH, 2),
                new SubjectRequirement(Category.PROGRAMMING, 1)
            };
            SemesterPlan plan4 = new SemesterPlan(subjects, req2, 12);
            printSubjects(planner.calculateSubjectList(plan4));
            // output: [Calculus(6,MATH), Linear Algebra(5,MATH), Java Programming(4,PROGRAMMING)]

            SubjectRequirement[] req3 = {
                new SubjectRequirement(Category.THEORY, 1)
            };
            SemesterPlan plan5 = new SemesterPlan(subjects, req3, 4);
            printSubjects(planner.calculateSubjectList(plan5));
            // output: [Discrete Math(4,THEORY)]

            try {
                SubjectRequirement[] reqImpossibleCategory = {
                    new SubjectRequirement(Category.PRACTICAL, 1)
                };
                SemesterPlan plan6 = new SemesterPlan(subjects, reqImpossibleCategory, 3);
                planner.calculateSubjectList(plan6);
                System.out.println("Case6: EXPECTED exception, but none thrown!");
            } catch (CryToStudentsDepartmentException e) {
                System.out.println("Case6: OK (CryToStudentsDepartmentException)");
            }

            try {
                SemesterPlan plan7 = new SemesterPlan(subjects, new SubjectRequirement[0], 100);
                planner.calculateSubjectList(plan7);
                System.out.println("Case7: EXPECTED exception, but none thrown!");
            } catch (CryToStudentsDepartmentException e) {
                System.out.println("Case7: OK (CryToStudentsDepartmentException)");
            }

            try {
                SubjectRequirement[] reqDup = {
                    new SubjectRequirement(Category.MATH, 1),
                    new SubjectRequirement(Category.MATH, 0)
                };
                SemesterPlan plan8 = new SemesterPlan(subjects, reqDup, 3);
                planner.calculateSubjectList(plan8);
                System.out.println("Case8: EXPECTED exception, but none thrown!");
            } catch (InvalidSubjectRequirementsException e) {
                System.out.println("Case8: OK (InvalidSubjectRequirementsException)");
            }

            UniversitySubject[] subjectsTie = {
                new UniversitySubject("S1", 8, 3, Category.MATH, 10),
                new UniversitySubject("S2", 8, 5, Category.PROGRAMMING, 8),
                new UniversitySubject("S3", 6, 5, Category.THEORY, 7)
            };
            SemesterPlan plan9 = new SemesterPlan(subjectsTie, new SubjectRequirement[0], 16);
            printSubjects(planner.calculateSubjectList(plan9));
            // output: [S1(8,MATH), S2(8,PROGRAMMING)]
        }

    }
//        UniversitySubject[] selectedSubjects = planner.calculateSubjectList(plan1);
//
//        int jarCount = planner.calculateJarCount(selectedSubjects, 11, 50);
//        System.out.println("Jar count: " + jarCount);
    //jarCount = 28


    private static void printSubjects(UniversitySubject[] subjects) {
        System.out.println(Arrays.toString(subjects));
    }

}