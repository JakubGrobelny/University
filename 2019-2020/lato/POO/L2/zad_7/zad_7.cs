// Pseudokod:

public class Student {
    public Course[] getCourses() {
        return courses;
    }
}

public class UsosWebPage {
    public void show() {
        pageLayout = GradeController.getStudentGradeInfo();
    }
}

public class GradeController {
    public static PageLayout getStudentGradeInfo() {
        var studentCourses = student.getCourses();

        foreach (var course in studentCourses) {
            mark = course.getMark(student);
        }

        return pageLayout;
    }
}

public class Course {
    private Mark getValue() {
    }

    public Mark getMark(Student student) {
        getValue();
    }
}

// Pytanie: 
//      Czy na podstawie diagramu można napisać jednoznaczny i precyzyjny kod?
// Odpowiedź: 
//      Nie, bo diagram pomija rzeczy takie jak typy i dokładną implementację
//      wywoływanych metod.
