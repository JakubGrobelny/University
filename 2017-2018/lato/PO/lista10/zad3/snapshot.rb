class Snapshot
    def self.run(formatter)
        throw :invalid_formatter unless formatter.respond_to?("format")

        for c in classes
            objects[c] = ObjectSpace.each_object(c).to_a
        end

        formatter.format(get_subclasses(Class))
    end

    def self.get_objects(c)
        throw :invalid_foramtter unless c.kind_of?(Class)
        return ObjectSpace.each_object(c).to_a
    end

    private
    def self.get_subclasses(c)
        classes = ObjectSpace.each_object(c).select {|sc| sc < c}
        tree = []
        
        if classes.size > 0
            for cl in classes
                tree.push([cl, get_subclasses(c)])
            end
        end

        return tree
    end
end