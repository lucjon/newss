# Function below from Chris Jefferson, from the GAP GitHub issue
# https://github.com/gap-system/gap/issues/695#issuecomment-287372410
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
