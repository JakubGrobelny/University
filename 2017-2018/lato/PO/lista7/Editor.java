// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L7, Edytor obiektów.
// Abstrakcyjna klasa „Editor”.
// Editor.java
// 2018-04-14

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

// Abstrakcyjna klasa, z której dziedzicz¹ klasy, które s³u¿¹
// do edycji obiektów. Implementuje interfejs „ActionListener”.
public abstract class Editor implements ActionListener
{
    protected Container container;
    public abstract void actionPerformed(ActionEvent e);
}