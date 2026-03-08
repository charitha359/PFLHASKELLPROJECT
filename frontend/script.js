const API = "https://bug-free-carnival-pj4qp55w6x7fr6wx-3000.app.github.dev/"

// Count
fetch(API + "/count")
.then(res => res.json())
.then(data => {
document.getElementById("count").innerText = data
})

// Top waste country
fetch(API + "/top-waste")
.then(res => res.json())
.then(data => {
document.getElementById("topCountry").innerText =
data.country + " (" + data.waste_tons + ")"
})

// Average waste
fetch(API + "/average-waste")
.then(res => res.json())
.then(data => {
document.getElementById("avgWaste").innerText = data
})

// Region summary graph
fetch(API + "/region-summary")
.then(res => res.json())
.then(data => {

const labels = data.map(d => d[0])
const values = data.map(d => d[1])

new Chart(document.getElementById("regionChart"),{

type:"bar",

data:{
labels:labels,
datasets:[{
label:"Waste by Region",
data:values
}]
}

})

})

// Top N countries
function loadTopN(){

const n = document.getElementById("topN").value

fetch(API + "/top?n=" + n)
.then(res => res.json())
.then(data => {

const labels = data.map(d => d.country)
const values = data.map(d => d.waste_tons)

new Chart(document.getElementById("topChart"),{

type:"bar",

data:{
labels:labels,
datasets:[{
label:"Top Countries Waste",
data:values
}]
}

})

})

}

// Country search
function searchCountry(){

const name = document.getElementById("countryName").value

fetch(API + "/country?name=" + name)
.then(res => res.json())
.then(data => {

const labels = data.map(d => d.country)
const values = data.map(d => d.waste_tons)

new Chart(document.getElementById("countryChart"),{

type:"bar",

data:{
labels:labels,
datasets:[{
label:"Country Waste",
data:values
}]
}

})

})

}