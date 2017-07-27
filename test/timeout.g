Fork_CallWithTimeout := function ( timeout, func, args... )
    local  ret;
    ret := ParDoByFork( [ func ], [ args ], rec(
          TimeOut := rec(
              tv_sec := timeout,
              tv_usec := 0 ) ) );
    if ret = [  ]  then
        return [ false ];
    else
        return [ true, ret[1] ];
    fi;
    return;
end;
