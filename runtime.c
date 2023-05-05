// #undef __STDC__
#include <stdio.h>


long long *initArray(long long size, long long init)
{long long i;
 long long *a = (long long *)malloc(size*sizeof(long long));
 for(i=0;i<size;i++) a[i]=init;
 return a;
}

long long *allocRecord(long long size)
{long long i;
 long long *p, *a;
 p = a = (long long *)malloc(size);
 for(i=0;i<size;i+=sizeof(long long)) *p++ = 0;
 return a;
}

struct string {long long length; unsigned char chars[1];};

long long stringEqual(struct string *s, struct string *t)
{long long i;
 if (s==t) return 1;
 if (s->length!=t->length) return 0;
 for(i=0;i<s->length;i++) if (s->chars[i]!=t->chars[i]) return 0;
 return 1;
}

void print(struct string *s)
{long long i; unsigned char *p=s->chars;
 for(i=0;i<s->length;i++,p++) putchar(*p);
}

void flush()
{
 fflush(stdout);
}

struct string consts[256];
struct string empty={0,""};

long long main()
{long long i;
 for(i=0;i<256;i++)
   {consts[i].length=1;
    consts[i].chars[0]=i;
   }
 return tigermain(0 /* static link!? */);
}

long long ord(struct string *s)
{
 if (s->length==0) return -1;
 else return s->chars[0];
}

struct string *chr(long long i)
{
 if (i<0 || i>=256)
   {printf("chr(%d) out of range\n",i); exit(1);}
 return consts+i;
}

long long size(struct string *s)
{
 return s->length;
}

struct string *substring(struct string *s, long long first, long long n)
{
 if (first<0 || first+n>s->length)
   {printf("substring([%d],%d,%d) out of range\n",s->length,first,n);
    exit(1);}
 if (n==1) return consts+s->chars[first];
 {struct string *t = (struct string *)malloc(sizeof(long long)+n);
  long long i;
  t->length=n;
  for(i=0;i<n;i++) t->chars[i]=s->chars[first+i];
  return t;
 }
}

struct string *concat(struct string *a, struct string *b)
{
 if (a->length==0) return b;
 else if (b->length==0) return a;
 else {long long i, n=a->length+b->length;
       struct string *t = (struct string *)malloc(sizeof(long long)+n);
       t->length=n;
       for (i=0;i<a->length;i++)
	 t->chars[i]=a->chars[i];
       for(i=0;i<b->length;i++)
	 t->chars[i+a->length]=b->chars[i];
       return t;
     }
}

long long not(long long i)
{ return !i;
}

// #undef getchar

struct string *getchartiger()
{long long i=getc(stdin);
 if (i==EOF) return &empty;
 else return consts+i;
}
