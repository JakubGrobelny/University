# Jakub Grobelny
# Pracownia PO, czwartek, s. 108
# L9z3, Wykresy funkcji.
# Rozszerzenie klasy „Function” o metodê rysuj¹c¹ wykres.
# plot.rb
# 2018-05-15

class ::Function

    def minmax(b, e)
        max = value(b)
        min = max
        @@iter = 0.1
        x = b
        while x < e
            val  = value(x)

            if val > max
                max = val
            elsif val < min
                min = val
            end

            x += @@iter
        end
        return [min, max]
    end

    def draw(b, e, thickness)
        @@width = 120
        @@height = 40

        iter_x = (e - b).to_f / @@width

        min_max = minmax(b, e)
        b_vert = min_max[0]
        e_vert = min_max[1]

        b_vert = b_vert < 0.0 ? b_vert : -2.0
        e_vert = e_vert > 0.0 ? e_vert : 2.0

        y_range = e_vert - b_vert

        iter_y = y_range / @@height

        y = e_vert
        real_y = @@height/2
        
        while y > b_vert
            x = b
            real_x = -@@width/2
            while x < e
                if real_y == 0
                    if equals?(x - x.round.to_i, 0.0, 0.03)
                        print '|'
                    else
                        print '-'
                    end
                elsif real_x == 0
                    if equals?(y - y.round.to_i, 0.0, 0.03)
                        print '-'
                    else
                        print '|'
                    end
                elsif equals?(y, value(x), thickness)
                    print '*'
                else
                    print ' '
                end
                x += iter_x
                real_x += 1
            end
            y -= iter_y
            real_y -= 1
            puts
        end

    end
end