// Jakub Grobelny
// Pracownia PO, czwartek, s. 108
// L7, Edytor obiektów.
// Implementacja edytora obiektów klasy „Circle”.
// CircleEditor.java
// 2018-04-14

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

// Klasa obs³uguj¹ca edytor obiektów klasy „Trójk¹t”.
public class CircleEditor extends Editor
{
    private Circle object;
    private JTextField name;
    private JTextField radius;
    private JTextField posX;
    private JTextField posY;
    private String fileName;

    private void updateObject(String name, double posX, double posY, double r)
    {
        this.object.setName(name);
        this.object.setPosX(posX);
        this.object.setPosY(posY);
        this.object.setRadius(r);
    }

    public void actionPerformed(ActionEvent e)
    {
        String name;
        double posX;
        double posY;
        double radius;

        try
        {
            name = this.name.getText();
            posX = Double.parseDouble(this.posX.getText());
            posY = Double.parseDouble(this.posY.getText());
            radius = Double.parseDouble(this.radius.getText());

            updateObject(name, posX, posY, radius);

            this.object.writeToFile(fileName);
        }
        catch (Exception exc)
        {
            JFrame frame = new JFrame();
            JOptionPane.showMessageDialog(frame, 
                                    "Given unput was invalid!",
                                    "Error!",
                                    JOptionPane.ERROR_MESSAGE);
        }
    }

    public CircleEditor(JFrame frame, String path)
    {
        this.object = new Circle();
        this.fileName = path;

        this.object.readFromFile(this.fileName);

        this.container = frame.getContentPane();

        this.name = new JTextField(this.object.getName(), 80);
        this.posX = new JTextField(Double.toString(this.object.getPosX()));
        this.posY = new JTextField(Double.toString(this.object.getPosY()));
        this.radius = new JTextField(Double.toString(this.object.getRadius()));
        JButton save = new JButton("Save");
        save.addActionListener(this);

        container.setPreferredSize(new Dimension(640, 240));
        container.setLayout(new GridLayout(4, 3));

        container.add(new JLabel("Name: "));
        container.add(this.name);
        container.add(new JLabel(""));

        container.add(new JLabel("Radius: "));
        container.add(this.radius);
        container.add(new JLabel(""));

        container.add(new JLabel("(x, y)"));
        container.add(this.posX);
        container.add(this.posY);

        container.add(new JLabel(""));
        container.add(save);
    }
}