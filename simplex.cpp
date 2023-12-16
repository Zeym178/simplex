#include <iostream>
#include <map>
#include <set>
#include <iomanip>
using namespace std;
map<string, map<string, double>> mp;
set<string> st;
string acn;

void mostrar(){
    cout << "\t";
    for(auto &it: st)
        cout << it << "\t";
    cout << endl;
    for(auto &it: mp){ 
        cout << it.first << "\t";
        for(auto &ot: it.second)
            cout << fixed << setprecision(2) <<  ot.second << "\t";
        cout << endl;
    }
    cout << endl;
}

void solve(int ac){
    while(true){
        string pi = "", li = "";
        double au = 1e9;
        for(auto &it: mp[acn])
            if(it.first != "b" && it.first != "Z" && it.second * ac < au)
                pi = it.first, au = it.second * ac;
        if(au >= -1e-10)
            return;
        cout << "Columna Pivote: " << pi << endl;
        au = 1e9;
        for(auto &it: mp)
            if(it.first != "max" && it.first != "min" && it.second[pi] > 0 && it.second["b"]/it.second[pi] < au)
                li = it.first, au = it.second["b"]/it.second[pi];
        cout << "Fila Pivote: " << li << "\n" << endl;
        mostrar();
        map<string, double> mpaux = mp[li];
        for(auto &it: mp[li])
            mpaux[it.first] /= mp[li][pi];
        mp.erase(li);
        mp[pi] = mpaux;
        for(auto &it: mp)
            if(it.first != pi){
                double vaux = it.second[pi] * -1;
                for(auto &ot: it.second)
                    ot.second += vaux * mp[pi][ot.first];
            } 
    }
}

void ciaoart(){
    map<string, double> aux = mp[acn];
    for(auto &it: mp[acn])
        if(it.first != "Z" && it.first[0] != 'z'){
            it.second = 0;
            for(auto &ot: mp)
                if(ot.first[0] == 'z')
                    it.second += ot.second[it.first];
        }
    cout << "Metodo: Dos fases " << endl;
    cout << "Fase 1 - minimización de tabla auxiliar" << "\n" << endl;
    solve(-1);
    set<string> stau = st;
    for(auto &it: stau)
        if(it[0] == 'z')
            st.erase(it);
    mp[acn] = aux;
    for(auto &it: mp)
        for(auto &ot: stau)
            if(ot[0] == 'z')
                (it.second).erase(ot);
    for(auto &it: mp)
        if(it.first != acn)
            for(auto &ot: mp)
                if(ot.first != it.first && mp[ot.first][it.first] != 0){
                    double ra = mp[ot.first][it.first];
                    for(auto &ok: mp[ot.first])
                        ok.second -= mp[it.first][ok.first] * ra;
                }
    cout << "Fase 2 - " << (acn == "max" ? "maximización" : "minimización");
    cout << " - sin variables Artificiales (Zi) " << "\n" << endl;
}

void rest(){
    string s;
    cin >> acn;
    mp[acn]["Z"] = 1;
    mp[acn]["b"] = 0;
    int ac = 0, mr = -1, art = 0;
    while(getline(cin, s)){
        if(s == "") continue;
        int sg = 1, r, auxart = 0; char c;
        double t = 1, f = 1;
        string au1 = "", au2 = "";
        bool var = 0;
        for(int i=0; i<s.size(); i++){
            while(s[i] == ' ') i++;
            if(s[i] == 'x' || s[i] == 'X') 
                var = 1, i++;
            if(s[i] == '+' || s[i] == '-' || s[i] == '<' || s[i] == '>' || s[i] == '=' || (!ac && i == s.size()-1)){
                if(i){
                    if(i == s.size()-1 && !ac)
                        (var ? au2 : au1) += s[i];
                    if(au1 != "")
                        t = stod(au1) * sg;
                    else 
                        t = 1;
                    r = stod(au2);
                    mr = max(mr, r);
                    if(!ac)
                        mp[acn]["x"+to_string(r)] = t*-1;
                    else
                        mp["y"+to_string(mr+1)]["x"+to_string(r)] = t;
                    au1 = "", au2 = "";
                }        
                if(s[i] == '<' || s[i] == '>' || s[i] == '='){
                    int eq = 0;
                    if(s[i] == '>') eq = 1;
                    if(s[i] == '<')
                        mr++, mp["y"+to_string(mr)]["y"+to_string(mr)] = 1, eq = -1; 
                    if(s[i] == '>' || s[i] == '='){ 
                        mr++, mp["z"+to_string(mr+(s[i] == '>'))] = mp["y"+to_string(mr)];
                        mp.erase("y"+to_string(mr));
                        if(s[i] == '>') mr++, mp["z"+to_string(mr)]["y"+to_string(mr-1)] = -1;
                        mp["z"+to_string(mr)]["z"+to_string(mr)] = 1, auxart++, art++;
                    }
                    i++;
                    if(s[i] == '=') i++;
                    while(s[i] == ' ') i++;
                    if(s[i] == '-'){
                        for(auto &it: mp[(auxart ? "z" : "y")+to_string(mr)])
                            it.second *= -1;
                        if(eq == 1){     
                            mp["y"+to_string(mr-1)] = mp["z"+to_string(mr)];
                            mp["y"+to_string(mr-1)].erase("z"+to_string(mr));
                            mp.erase("z"+to_string(mr));
                            auxart--, art--, mr--;
                        }else if(eq == -1){
                            mp["z"+to_string(mr+1)] = mp["y"+to_string(mr)];
                            mp["z"+to_string(mr+1)]["z"+to_string(mr+1)] = 1;
                            mp.erase("y"+to_string(mr));
                            auxart++, art++, mr++;
                        }else
                            mp["z"+to_string(mr)]["z"+to_string(mr)] *= -1;
                        i++;
                    }
                    sg = 1;
                    f = stod(s.substr(i)) * sg;
                }
                sg = (s[i] == '-' ? -1 : 1);
                var = 0;
            }else
                (var ? au2 : au1) += s[i];
        }
        if(ac)
            mp[(auxart ? "z" : "y") +to_string(mr)]["b"] = f;
        ac++;
    }
    for(auto &it: mp)
        for(auto &ot: it.second)
            st.insert(ot.first);
    for(auto &it: st)
        for(auto &ot: mp)
            if(!mp[ot.first][it])
                mp[ot.first][it] = 0;
    cout << "Forma Padrón" << "\n" << endl;
    mostrar();
    if(art)
       ciaoart();
}

int main(){
    rest();     
    solve((acn == "max" ? 1 : -1));
    for(auto &it: mp)
        if(it.first != acn)
            cout << it.first << " = " << it.second["b"] << endl;
        else
            cout << acn << " Z = " << it.second["b"] << endl;
    mostrar();
    cout << "y: variable de holgura, z: variable artificial" << endl;
    
    return 0;
}