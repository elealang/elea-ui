

// Later the server-browser TS package will handle the actual storage of
// the computers (e.g. keeping this data in local storage).  this is wrong. 
// will be stored in memory on each request. computers can manage local storage though
// the computers individually though should handle storage of their own data.
//   this will be made explicit in the dependencies section 
//   just as the servers are explicitly dependent on the same local storage
//      for computers
//
// server = args -> computer (computer computer)
// computer = something that you can get or put (replace), args -> [x]
// story computer = a story that you can get or put, which is enabled by the server., args -> (args -> story)


class Story {
  constructor(name) {
    this.name   = name;
    this.arrows = [];
  }
}


const EleaStoryComputer = {
    getStory: function(computerId) {
        if (localStorage.getItem(computerId) === null) {
            console.log("here")
            return new Story("my-dev-story");
        } else {
            console.log("there")
            return JSON.parse(localStorage.getItem(computerId));
        }
    },
    putStory: function(computerId, story) {
        localStorage.setItem(computerId, story);
    },
    appendToStory: function(computerId, arrow) {
        const story = this.getStory(computerId);
        console.log(story);
        story.arrows.push(arrow);
        localStorage.setItem(computerId, JSON.stringify(story));
    }
};

const BrowserServer = {
    getComputer: function(computerId) {
        return this.computers[computerId];
    },
    computers: {
        "computer.elea.software/story": EleaStoryComputer
    } 

};


window.Elea = {
    server: BrowserServer, 
};


window.addEventListener("DOMContentLoaded", (event) => {
    const story = Elea.server.getComputer("computer.elea.software/story").getStory("my-dev-story");

    console.log(localStorage);

    const arrowsElem = document.querySelector(".comp-story-history-arrows");

    console.log(story);

    story.arrows.forEach((arrow, index) => {
        const arrowElem = document.createElement("div");
        arrowElem.classList.add("comp-story-history-arrow");

        const arrowIndexElem = document.createElement("div");
        arrowIndexElem.classList.add("comp-story-history-arrow-index");
        arrowIndexElem.innerText = index;

        const arrowNameElem = document.createElement("div");
        arrowNameElem.classList.add("comp-story-history-arrow-name");
        arrowNameElem.innerText = arrow.name.toUpperCase();

        arrowElem.append(arrowIndexElem);
        arrowElem.append(arrowNameElem);

        console.log(arrowElem);

        arrowsElem.append(arrowElem);
    }); 
});

