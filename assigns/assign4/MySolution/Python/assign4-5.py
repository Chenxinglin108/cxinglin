def string_fset_at(cs, i0, c0):
    if 0 <= i0 < len(cs):
       
        return cs[:i0] + c0 + cs[i0 + 1:]
    else:
       
        return cs
    
    
    
    
    
alphabet = ''.join([chr(ord('a') + i) for i in range(26)])






def list_of_buddies(word):
    n0 = len(word)
    
    def work():
        result = []
        for i0 in range(n0):
            c0 = word[i0]
            for c1 in alphabet:
                if c1 != c0:
                    result.append(word[:i0] + c1 + word[i0+1:])
        return result

    return work()

