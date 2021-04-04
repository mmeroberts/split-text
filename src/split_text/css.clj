(ns split-text.css)



(def main-css
  "
    <style>

    @media print {
                  .sidenav {
                            display: none;
                            }
                  .openBtn{
                           display: none;
                           }
                  body {
                        margin-left: 5%;
                                     margin-right: 5%;
                        }
                  }

    body {
          margin-left: 10%;
                       margin-right: 10%;
          border: none;
          /*overflow-wrap: break-word;*/
          }


    ol {
        padding: 0;
        margin-left: 1em;
        }

    @media only screen and (orientation: landscape) {
                                                     body {
                                                           margin-left: 10%;
                                                           margin-right: 10%;
                                                           }
                                                     }
* {
  box-sizing: border-box;
}
.row {
  display: flex;
}

/* Create two equal columns that sits next to each other */
.column {
  flex: 50%;
  padding: 10px;

}

    .h1-english, .h1-wylie {
                 font-size: large;
                 font-weight: bold;
                 text-align: center;
                 display: block;
                 }
    .h1-bo  {
             font-size: xx-large;
             font-weight: bold;
             text-align: center;
             /*display: block;*/
             }
    .h2-english, .h2-wylie {
                 font-size: large;
                 font-weight: normal;
                 font-family: 'Calibri', sans-serif;
                 text-align: center;
                 margin-bottom: 2em;
                 /*display: block;*/
                 }
    .h2-bo {
            font-size: xx-large;
            font-weight: normal;
            text-align: center;
            /*margin-bottom: 0; !* was 2em *!*/
            }

    .h3-wylie, .h3-english {
                 font-size: large;
                 font-weight: bold;
                 margin-block-start: 12px !important;
                 }
    .h3-bo {
            font-size: 1.6em;
            font-weight: bold;
            /*display: block;*/
            }
    .h5-english , .h5-wylie{
                 font-size: small;
                 font-weight: bold;
                 text-align: center;
                 margin-bottom: 2em;
                 /*display: block;*/
                 }
    .h5-bo {
            font-size: xx-large;
            font-weight: normal;
            text-align: center;
            /*margin-bottom: 0; !* was 2em *!*/
            }


    .vn {
         vertical-align: super;
         font-size: 8pt;
         }
    .v-bo {
           font-size: 18pt;
           margin-block-start: 1em !important;
           margin-block-end: 0 !important;
           }
    .nameHighlight {
                    color: MediumBlue;
                    }
    .vq-bo {
            font-size: 18pt;
            padding-left: 50px;
            margin-top: 0;
            margin-bottom: 0;
            margin-block-start: 0 !important;
            margin-block-end: 0 !important;
            }
    .vq-wylie, .vq-english {
                 font-size: 12pt;
                 padding-left: 50px;
                 margin-top: 0;
                 margin-bottom: 0;
                 }
    .v-wylie, .v-english {
                font-size: 12pt;
                }
    .v-back {
             font-size: 12pt;
             color: rebeccapurple;
             }
    .u-bo {
           color: purple;
           }

    .title {
            font-size: xx-large;
            font-weight: bold;
            }
    .subtitle {
               font-size: large;
               font-weight: bold;
               }

    .first-page {
                 text-align: center;
                 }
    .permission {
                 font-size: large;
                 color: red;
                 }

    .copyright {
                font-size: large;
                }
    .logo {
           text-align: center;
           margin: 1em;
           }
    .center-logo {
                  text-align: center;
                  }
    h1 {
        font-size: 18pt;
        font-weight: bold;
        text-align: center;

        }



    h2 {
        font-size: 16pt;
        /*font-weight: bold;*/
        text-align: center;
        margin-bottom:  0;
        margin-top:  0;

        }
    h3 {
        font-size: 13pt;
        /*font-weight: bold;*/
        margin-bottom:  0;
        margin-top:  1em;
        margin-block-start: 10px !important;
        margin-block-end: 0 !important;
        }
    h5 {
        font-size: 8pt;
        /*font-weight: bold;*/
        text-align: center;
        margin-bottom:  0;
        margin-top:  0;

        }
    p {
       font-size: 15pt;
       margin-top:  0;

       }
    span.underline {
                    color: blue;
                    text-decoration: none;
                    }
    span.bo-brackets {
                      font-family: 'Calibri', sans-serif;
                      font-size: 10pt;
                      }

    span.bo-ref {
                 vertical-align: super;
                 font-size: 8pt;
                 }
    .footer {
             align-content: center;
             }


    .p-bo {
           font-size: 15pt;
           margin-top:  0;
           margin-block-start: 1em !important;
           margin-block-end: 0 !important;
           }
     .p-english, .p-wylie {
           font-size: 15pt;
           margin-top:  0;
           margin-block-start: 1em !important;
           margin-block-end: 0 !important;
           }


    .pq-bo {
            font-size: 15pt;
            margin-top:  0;
            margin-block-start: 0 !important;
            margin-block-end: 0 !important;
            }

.parallel:after {
  display: table;
  clear: both;
             }

.parallel .p-bo {
              width: 48%;
              float: left;
              /*border: 3px solid #00AF50;*/
              padding: 0px;
              margin-block-start: 0 !important;
              margin-block-end: 0 !important;
              }

.parallel .q-bo {
              width: 48%;
              float: left;
              font-size: 18pt;
              /*border: 3px solid #00AF50;*/
              padding: 0px;
              }
.q-bo {
       margin: 0;
       margin-block-start: 0 !important;
       margin-block-end: 0 !important;
       }

.q-english p {
              margin: 0;
              }

.q-bo p {
         margin: 0;
         }
.parallel  .p-english {
                   width: 48%;
                    float: left;
                   /*border: 3px solid #1FAF50;*/
                   padding: 0px;
                   }

.parallel  .q-english {
                   width: 48%;
                    float: left;
                          /*border: 3px solid #1FAF50;*/
                   padding: 0px;
                   }

.parallel .h2-bo {
               width: 48%;

               /*border: 3px solid red;*/
               padding: 0px;
               }
.parallel .h2-english {
                    width: 48%;
                    float: right;
                    /*border: 3px solid darkred;*/
                    padding: 0px;
                    }

.parallel .h5-bo {
               width: 48%;

               /*border: 3px solid red;*/
               padding: 0px;
               }
.parallel .h5-english {
                    width: 48%;
                    float: right;
                    /*border: 3px solid darkred;*/
                    padding: 0px;
                    }

.parallel .h3-bo {
               width: 48%;

               /*border: 3px solid gold;*/
               padding: 0px;
               }
.parallel .h3-english {
                    width: 48%;
                    float: right;
                    /*border: 3px solid yellow;*/
                    padding: 0px;
                    }

/* The side navigation menu */
.sidenav {
    height: 100%; /* 100% Full-height */
            width: 0; /* 0 width - change this with JavaScript */
            position: fixed; /* Stay in place */
            z-index: 1; /* Stay on top */
            top: 0; /* Stay at the top */
            left: 0;
            background-color: #111; /* Black*/
            overflow-x: hidden; /* Disable horizontal scroll */
            /*padding-top: 60px; !* Place content 60px from the top *!*/
            transition: 0.5s; /* 0.5 second transition effect to slide in the sidenav */
    }

/* The navigation menu links */
.sidenav .showTextEng {
                                                      padding: 8px 8px 8px 32px;
                                                               text-decoration: none;
                                                               font-size: 12px;
                                                               color: #818181;
                                                               display: none;
                                                               transition: 0.3s;
                                                      }
.sidenav .hideTextEng {
                       padding: 8px 8px 8px 32px;
                                text-decoration: none;
                                font-size: 12px;
                                color: #818181;
                                display: block;
                                transition: 0.3s;
                       }
.sidenav .hideTextBo {
                      padding: 8px 8px 8px 32px;
                               text-decoration: none;
                               font-size: 12px;
                               color: #818181;
                               display: block;
                               transition: 0.3s;
                      }
.sidenav .showTextBo {
                      padding: 8px 8px 8px 32px;
                               text-decoration: none;
                               font-size: 12px;
                               color: #818181;
                               display: none;
                               transition: 0.3s;
                      }

/* The navigation menu links */
.sidenav a {
     padding: 8px 8px 8px 32px;
              text-decoration: none;
              font-size: 25px;
              color: #818181;
              display: block;
              transition: 0.3s;
     }




/* When you mouse over the navigation links, change their color */
.sidenav a:hover {
        color: #f1f1f1;
                   }

/* Position and style the close button (top right corner) */
.sidenav .closebtn {
                                                                                                                                      position: absolute;
                                                                                                                                      top: 0;
                                                                                                                                      right: 25px;
                                                                                                                                      font-size: 36px;
                                                                                                                                      margin-left: 50px;
                                                                                                                                      }

/* Style page content - use this if you want to push the page content to the right when you open the side navigation */
#main {
                                                                                                                              transition: margin-left .5s;
                                                                                                                                          padding: 20px;
                                                                                                                              }

/* On smaller screens, where height is less than 450px, change the style of the sidenav (less padding and a smaller font size) */
@media screen and (max-height: 450px) {
                                                                                                                                   .sidenav {padding-top: 15px;}
                                                                                                                                    .sidenav a {font-size: 18px;}
}

</style>")


