class Collection
    class Element
        def initialize(value, prev, next)
            @value = value
            @prev  = prev
            @next  = next
        end

        def val
            @value
        end

        def next
            @next
        end

        def prev
            @prev
        end
    end

    def initialize()
        @first = nil
        @last  = nil
        @length = 0
    end

    def length
        @length
    end

    def insert(value)
        @length += 1
        if @first == nil
            element = Element.new(value, nil, nil)
            @first = element
            @last  = element
        else
            ptr = @first
            if ptr.next == nil
                if ptr.val > value
                    element = Element.new(value, nil, ptr)
                    @first = element
                    ptr.prev  = element
                else
                    element = Element.new(value, ptr, nil)
                    @last = element
                    ptr.next = element
                end
            else
                ptr = ptr.next
                ptr_prev = @first
                while ()

        end

    def remove(value)
        #TODO: change length
    end

end