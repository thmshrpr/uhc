package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

import java.util.* ;
import java.io.* ;

/**
 * Function accepting/expecting 1 parameter(s).
 * @see uu.jazy.core.Function
 */
public abstract class Function1 extends Function
{
    public Function1( )
    {
        nrParams = 1 ;
    }
    
    public Function1( String nm )
    {
        this() ;
        setName ( nm ) ;
    }
    
    public Function1( Function prev, String nm )
    {
        this( prev.getName() + "." + nm ) ;
    }
    
    abstract protected Object eval1( Object v1 ) ;

    public Apply apply1( Object v1 )
    {
        return new Apply1F1( this, v1 ) ;
    }
    
    public Apply apply2( Object v1, Object v2 )
    {
        return apply1( v1 ).apply1( v2 ) ;
    }
    
    public Apply apply3( Object v1, Object v2, Object v3 )
    {
        return apply1( v1 ).apply2( v2, v3 ) ;
    }
    
    public Apply apply4( Object v1, Object v2, Object v3, Object v4 )
    {
        return apply1( v1 ).apply3( v2, v3, v4 ) ;
    }
    
    public Apply apply5( Object v1, Object v2, Object v3, Object v4, Object v5 )
    {
        return apply1( v1 ).apply4( v2, v3, v4, v5 ) ;
    }
    
}

class Apply1F1 extends Apply1
{
	public Apply1F1( Object f, Object p1 )
	{
		super( f, p1 ) ;
	}
	
    protected void evalSet()
    {
        funcOrVal = ((Function1)funcOrVal).eval1( p1 ) ;
        p1 = null ;
    }
    
}

