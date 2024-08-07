        program prog
        open(1, access='direct', file='exercise_open.tmp', status='unknown', iostat=ios, recl=4)
        write(6, *) (ios .eq. 0)
        end
