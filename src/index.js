import './main.css';
import { Elm } from './Main.elm';
import Github from './github';
var app = Elm.Main.init({
  node: document.getElementById('root'),
});

var github = new Github();

function searchGithub(query) {
  github.getSearch(query).repositories({}, function (err, repositories) {
    app.ports.githubResponse.send(repositories);
  });
}

app.ports.githubSearch.subscribe(searchGithub);
