class FormatTXT
    def self.format(class_tree)
        if class_tree.size == 1
            return class_tree[0].to_s
        else
            return "<font size=\"10\"><b>" + class_tree[0].to_s + "</b></font>" + \
                   "\n\t<b> Instances: </b> \n" +

    end
end