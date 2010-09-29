// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.new_replication = function(debug) {

  if (debug) debugger;

  var host = CouchDB.host;
  var sourceDb = new CouchDB("test_suite_db_a",{"X-Couch-Full-Commit":"false"});
  var targetDb = new CouchDB("test_suite_db_b",{"X-Couch-Full-Commit":"false"});

  var dbPairs = [
    {
      source: sourceDb.name,
      target: targetDb.name
    },
    {
      source: "http://" + host + "/" + sourceDb.name,
      target: targetDb.name
    },
    {
      source: sourceDb.name,
      target: "http://" + host + "/" + targetDb.name
    },
    {
      source: "http://" + host + "/" + sourceDb.name,
      target: "http://" + host + "/" + targetDb.name
    }
  ];

  var att1_data = CouchDB.request("GET", "/_utils/script/test/lorem.txt");
  att1_data = att1_data.responseText;

  var att2_data = CouchDB.request("GET", "/_utils/script/test/lorem_b64.txt");
  att2_data = att2_data.responseText;

  var sourceInfo, targetInfo;
  var docs, doc, copy;
  var repResult;
  var i, j, k;


  function populateDb(db, docs, dontRecreateDb) {
    if (dontRecreateDb !== true) {
      db.deleteDb();
      db.createDb();
    }
    for (var i = 0; i < docs.length; i++) {
      var doc = docs[i];
      delete doc._rev;
    }
    if (docs.length > 0) {
      db.bulkSave(docs);
    }
  }


  function addAtt(db, doc, attName, attData, type) {
    var uri = "/" + db.name + "/" + encodeURIComponent(doc._id) + "/" + attName;

    if (doc._rev) {
      uri += "?rev=" + doc._rev;
    }

    var xhr = CouchDB.request("PUT", uri, {
      headers: {
        "Content-Type": type
      },
      body: attData
    });

    T(xhr.status === 201);
    doc._rev = JSON.parse(xhr.responseText).rev;
  }


  function compareObjects(o1, o2) {
    for (var p in o1) {
      if (o1[p] === null && o2[p] !== null) {
        return false;
      } else if (typeof o1[p] === "object") {
        if ((typeof o2[p] !== "object") || o2[p] === null) {
          return false;
        }
        if (!arguments.callee(o1[p], o2[p])) {
          return false;
        }
      } else {
        if (o1[p] !== o2[p]) {
          return false;
        }
      }
    }
    return true;
  }


  function waitForSeq(sourceDb, targetDb) {
    var targetSeq,
        sourceSeq = sourceDb.info().update_seq,
        t0 = new Date(),
        t1,
        ms = 1000;

    do {
      targetSeq = targetDb.info().update_seq;
      t1 = new Date();
    } while (((t1 - t0) <= ms) && targetSeq < sourceSeq);
  }


  function wait(ms) {
    var t0 = new Date(), t1;
    do {
      CouchDB.request("GET", "/");
      t1 = new Date();
    } while ((t1 - t0) <= ms);
  }


  // test simple replications (not continuous, not filtered), including
  // conflict creation
  docs = makeDocs(1, 21);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    value: "ddoc"
  });

  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    // add some attachments
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "readme.txt", att1_data, "text/plain");
    }

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    T(typeof repResult.session_id === "string");
    T(repResult.source_last_seq === sourceInfo.update_seq);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 1);
    T(repResult.history[0].session_id === repResult.session_id);
    T(typeof repResult.history[0].start_time === "string");
    T(typeof repResult.history[0].end_time === "string");
    T(repResult.history[0].start_last_seq === 0);
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === sourceInfo.doc_count);
    T(repResult.history[0].missing_found === sourceInfo.doc_count);
    T(repResult.history[0].docs_read === sourceInfo.doc_count);
    T(repResult.history[0].docs_written === sourceInfo.doc_count);
    T(repResult.history[0].doc_write_failures === 0);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      T(compareObjects(doc, copy) === true);

      if (j >= 10 && j < 15) {
        var atts = copy._attachments;
        T(typeof atts === "object");
        T(typeof atts["readme.txt"] === "object");
        T(atts["readme.txt"].revpos === 2);
        T(atts["readme.txt"].content_type.indexOf("text/plain") === 0);
        T(atts["readme.txt"].stub === true);

        var att_copy = CouchDB.request(
          "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
        ).responseText;
        T(att_copy.length === att1_data.length);
        T(att_copy === att1_data);
      }
    }


    // add one more doc to source, more attachments to some existing docs
    // and replicate again
    var newDoc = {
      _id: "foo666",
      value: "d"
    };
    T(sourceDb.save(newDoc).ok);

    // add some more attachments
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "data.dat", att2_data, "application/binary");
    }

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    T(typeof repResult.session_id === "string");
    T(repResult.source_last_seq === sourceInfo.update_seq);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 2);
    T(repResult.history[0].session_id === repResult.session_id);
    T(typeof repResult.history[0].start_time === "string");
    T(typeof repResult.history[0].end_time === "string");
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 6));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 6);
    T(repResult.history[0].missing_found === 6);
    T(repResult.history[0].docs_read === 6);
    T(repResult.history[0].docs_written === 6);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(newDoc._id);
    T(copy !== null);
    T(copy._id === newDoc._id);
    T(copy.value === newDoc.value);

    for (j = 10; j < 15; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      T(compareObjects(doc, copy) === true);

      var atts = copy._attachments;
      T(typeof atts === "object");
      T(typeof atts["readme.txt"] === "object");
      T(atts["readme.txt"].revpos === 2);
      T(atts["readme.txt"].content_type.indexOf("text/plain") === 0);
      T(atts["readme.txt"].stub === true);

      var att1_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
      ).responseText;
      T(att1_copy.length === att1_data.length);
      T(att1_copy === att1_data);

      T(typeof atts["data.dat"] === "object");
      T(atts["data.dat"].revpos === 3);
      T(atts["data.dat"].content_type.indexOf("application/binary") === 0);
      T(atts["data.dat"].stub === true);

      var att2_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
      ).responseText;
      T(att2_copy.length === att2_data.length);
      T(att2_copy === att2_data);
    }

    // test deletion is replicated
    doc = sourceDb.open(docs[1]._id);
    T(sourceDb.deleteDoc(doc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.doc_del_count === targetInfo.doc_del_count);
    T(targetInfo.doc_del_count === 1);

    T(repResult.history instanceof Array);
    T(repResult.history.length === 3);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[1]._id);
    T(copy === null);

    var changes = targetDb.changes({since: 0});
    var idx = changes.results.length - 1;
    T(changes.results[idx].id === docs[1]._id);
    T(changes.results[idx].deleted === true);

    // test conflict
    doc = sourceDb.open(docs[0]._id);
    doc.value = "white";
    T(sourceDb.save(doc).ok);

    copy = targetDb.open(docs[0]._id);
    copy.value = "black";
    T(targetDb.save(copy).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    T(repResult.history instanceof Array);
    T(repResult.history.length === 4);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    T(copy._rev.indexOf("2-") === 0);
    T(copy._conflicts instanceof Array);
    T(copy._conflicts.length === 1);
    T(copy._conflicts[0].indexOf("2-") === 0);

    // replicate again with conflict
    doc.value = "yellow";
    T(sourceDb.save(doc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    T(repResult.history instanceof Array);
    T(repResult.history.length === 5);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    T(copy._rev.indexOf("3-") === 0);
    T(copy._conflicts instanceof Array);
    T(copy._conflicts.length === 1);
    T(copy._conflicts[0].indexOf("2-") === 0);

    // resolve the conflict
    T(targetDb.deleteDoc({_id: copy._id, _rev: copy._conflicts[0]}).ok);

    // replicate again, check there are no more conflicts
    doc.value = "rainbow";
    T(sourceDb.save(doc).ok);

    repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    T(repResult.history instanceof Array);
    T(repResult.history.length === 6);
    T(repResult.history[0].start_last_seq === (sourceInfo.update_seq - 1));
    T(repResult.history[0].end_last_seq === sourceInfo.update_seq);
    T(repResult.history[0].recorded_seq === sourceInfo.update_seq);
    T(repResult.history[0].missing_checked === 1);
    T(repResult.history[0].missing_found === 1);
    T(repResult.history[0].docs_read === 1);
    T(repResult.history[0].docs_written === 1);
    T(repResult.history[0].doc_write_failures === 0);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    T(copy._rev.indexOf("4-") === 0);
    T(typeof copy._conflicts === "undefined");
  }


  // test create_target option
  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    targetDb.deleteDb();

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {body: {create_target: true}}
    );
    T(repResult.ok === true);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);
    T(sourceInfo.update_seq === targetInfo.update_seq);
  }


  // test filtered replication
  docs = makeDocs(1, 31);
  docs.push({
    _id: "_design/mydesign",
    language: "javascript",
    filters: {
      myfilter: (function(doc, req) {
        var modulus = Number(req.query.modulus);
        var special = req.query.special;
        return (doc.integer % modulus === 0) || (doc.string === special);
      }).toString()
    }
  });

  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            modulus: 2,
            special: "7"
          }
        }
      }
    );

    T(repResult.ok === true);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if ((doc.integer && (doc.integer % 2 === 0)) || (doc.string === "7")) {

        T(copy !== null);
        T(compareObjects(doc, copy) === true);
      } else {
        T(copy === null);
      }
    }

    T(repResult.history instanceof Array);
    T(repResult.history.length === 1);
    // NOT 31 (31 is db seq for last doc - the ddoc, which was not replicated)
    T(repResult.source_last_seq === 30);
    T(repResult.history[0].start_last_seq === 0);
    T(repResult.history[0].end_last_seq === 30);
    T(repResult.history[0].recorded_seq === 30);
    // 16 => 15 docs with even integer field  + 1 doc with string field "7"
    T(repResult.history[0].missing_checked === 16);
    T(repResult.history[0].missing_found === 16);
    T(repResult.history[0].docs_read === 16);
    T(repResult.history[0].docs_written === 16);
    T(repResult.history[0].doc_write_failures === 0);


    // add new docs to source and resume the same replication
    var newDocs = makeDocs(50, 56);
    populateDb(sourceDb, newDocs, true);

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            modulus: 2,
            special: "7"
          }
        }
      }
    );

    T(repResult.ok === true);

    for (j = 0; j < newDocs.length; j++) {
      doc = newDocs[j];
      copy = targetDb.open(doc._id);

      if (doc.integer && (doc.integer % 2 === 0)) {

        T(copy !== null);
        T(compareObjects(doc, copy) === true);
      } else {
        T(copy === null);
      }
    }

    // last doc has even integer field, so last replicated seq is 36
    T(repResult.source_last_seq === 36);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 2);
    T(repResult.history[0].start_last_seq === 30);
    T(repResult.history[0].end_last_seq === 36);
    T(repResult.history[0].recorded_seq === 36);
    T(repResult.history[0].missing_checked === 3);
    T(repResult.history[0].missing_found === 3);
    T(repResult.history[0].docs_read === 3);
    T(repResult.history[0].docs_written === 3);
    T(repResult.history[0].doc_write_failures === 0);
  }


  // test filtered replication works as expected after changing the filter's
  // code (ticket COUCHDB-892)
  var filterFun1 = (function(doc, req) {
    if (doc.value < Number(req.query.maxvalue)) {
      return true;
    } else {
      return false;
    }
  }).toString();

  var filterFun2 = (function(doc, req) {
    return true;
  }).toString();

  for (i = 0; i < dbPairs.length; i++) {
    populateDb(targetDb, []);
    populateDb(sourceDb, []);

    T(sourceDb.save({_id: "foo1", value: 1}).ok);
    T(sourceDb.save({_id: "foo2", value: 2}).ok);
    T(sourceDb.save({_id: "foo3", value: 3}).ok);
    T(sourceDb.save({_id: "foo4", value: 4}).ok);

    var ddoc = {
      "_id": "_design/mydesign",
      "language": "javascript",
      "filters": {
        "myfilter": filterFun1
      }
    };

    T(sourceDb.save(ddoc).ok);

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params : {
            maxvalue: "3"
          }
        }
      }
    );

    T(repResult.ok === true);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 1);
    T(repResult.history[0].docs_written === 2);
    T(repResult.history[0].docs_read === 2);
    T(repResult.history[0].doc_write_failures === 0);

    var docFoo1 = targetDb.open("foo1");
    T(docFoo1 !== null);
    T(docFoo1.value === 1);

    var docFoo2 = targetDb.open("foo2");
    T(docFoo2 !== null);
    T(docFoo2.value === 2);

    var docFoo3 = targetDb.open("foo3");
    T(docFoo3 === null);

    var docFoo4 = targetDb.open("foo4");
    T(docFoo4 === null);

    // replication should start from scratch after the filter's code changed

    ddoc.filters.myfilter = filterFun2;
    T(sourceDb.save(ddoc).ok);

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params : {
            maxvalue: "3"
          }
        }
      }
    );

    T(repResult.ok === true);
    T(repResult.history instanceof Array);
    T(repResult.history.length === 1);
    T(repResult.history[0].docs_written === 3);
    T(repResult.history[0].docs_read === 3);
    T(repResult.history[0].doc_write_failures === 0);

    docFoo1 = targetDb.open("foo1");
    T(docFoo1 !== null);
    T(docFoo1.value === 1);

    docFoo2 = targetDb.open("foo2");
    T(docFoo2 !== null);
    T(docFoo2.value === 2);

    docFoo3 = targetDb.open("foo3");
    T(docFoo3 !== null);
    T(docFoo3.value === 3);

    docFoo4 = targetDb.open("foo4");
    T(docFoo4 !== null);
    T(docFoo4.value === 4);

    T(targetDb.open("_design/mydesign") !== null);
  }


  // test replication by doc IDs
  docs = makeDocs(1, 11);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    integer: 1
  });

  var target_doc_ids = [
    { initial: ["1", "2", "10"], after: [], conflict_id: "2" },
    { initial: ["1", "2"], after: ["7"], conflict_id: "1" },
    { initial: ["1", "foo_666", "10"], after: ["7"], conflict_id: "10" },
    { initial: ["_design/foo", "8"], after: ["foo_5"], conflict_id: "8" },
    { initial: ["_design%2Ffoo", "8"], after: ["foo_5"], conflict_id: "8" },
    { initial: [], after: ["foo_1000", "_design/foo", "1"], conflict_id: "1" }
  ];
  var doc_ids, after_doc_ids;
  var id, num_inexistent_docs, after_num_inexistent_docs;
  var total, after_total;

  for (i = 0; i < dbPairs.length; i++) {

    for (j = 0; j < target_doc_ids.length; j++) {
      doc_ids = target_doc_ids[j].initial;
      num_inexistent_docs = 0;

      for (k = 0; k < doc_ids.length; k++) {
        id = doc_ids[k];
        if (id.indexOf("foo_") === 0) {
          num_inexistent_docs += 1;
        }
      }

      populateDb(sourceDb, docs);
      populateDb(targetDb, []);

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: doc_ids
          }
        }
      );

      total = doc_ids.length - num_inexistent_docs;
      T(repResult.ok === true);
      T(typeof repResult.start_time === "string");
      T(typeof repResult.end_time === "string");
      T(repResult.docs_read === total);
      T(repResult.docs_written === total);
      T(repResult.doc_write_failures === 0);

      for (k = 0; k < doc_ids.length; k++) {
        id = decodeURIComponent(doc_ids[k]);
        doc = sourceDb.open(id);
        copy = targetDb.open(id);

        if (id.indexOf("foo_") === 0) {
          T(doc === null);
          T(copy === null);
        } else {
          T(doc !== null);
          T(copy !== null);
          T(compareObjects(doc, copy) === true);
        }
      }

      // be absolutely sure that other docs were not replicated
      for (k = 0; k < docs.length; k++) {
        var base_id = docs[k]._id;
        id = encodeURIComponent(base_id);
        doc = targetDb.open(base_id);

        if ((doc_ids.indexOf(id) >= 0) || (doc_ids.indexOf(base_id) >= 0)) {
            T(doc !== null);
        } else {
            T(doc === null);
        }
      }

      targetInfo = targetDb.info();
      T(targetInfo.doc_count === total);


      // add more docs throught replication by doc IDs
      after_doc_ids = target_doc_ids[j].after;
      after_num_inexistent_docs = 0;

      for (k = 0; k < after_doc_ids.length; k++) {
        id = after_doc_ids[k];
        if (id.indexOf("foo_") === 0) {
          after_num_inexistent_docs += 1;
        }
      }

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: after_doc_ids
          }
        }
      );

      after_total = after_doc_ids.length - after_num_inexistent_docs;
      T(repResult.ok === true);
      T(typeof repResult.start_time === "string");
      T(typeof repResult.end_time === "string");
      T(repResult.docs_read === after_total);
      T(repResult.docs_written === after_total);
      T(repResult.doc_write_failures === 0);

      for (k = 0; k < after_doc_ids.length; k++) {
        id = after_doc_ids[k];
        doc = sourceDb.open(id);
        copy = targetDb.open(id);

        if (id.indexOf("foo_") === 0) {
          T(doc === null);
          T(copy === null);
        } else {
          T(doc !== null);
          T(copy !== null);
          T(compareObjects(doc, copy) === true);
        }
      }

      // be absolutely sure that other docs were not replicated
      for (k = 0; k < docs.length; k++) {
        var base_id = docs[k]._id;
        id = encodeURIComponent(base_id);
        doc = targetDb.open(base_id);

        if ((doc_ids.indexOf(id) >= 0) || (after_doc_ids.indexOf(id) >= 0) ||
            (doc_ids.indexOf(base_id) >= 0) ||
            (after_doc_ids.indexOf(base_id) >= 0)) {
            T(doc !== null);
        } else {
            T(doc === null);
        }
      }

      targetInfo = targetDb.info();
      T(targetInfo.doc_count === (total + after_total));


      // replicate again the same doc after updated on source (no conflict)
      id = target_doc_ids[j].conflict_id;
      doc = sourceDb.open(id);
      T(doc !== null);
      doc.integer = 666;
      T(sourceDb.save(doc).ok);
      addAtt(sourceDb, doc, "readme.txt", att1_data, "text/plain");
      addAtt(sourceDb, doc, "data.dat", att2_data, "application/binary");

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: [id]
          }
        }
      );

      T(repResult.ok === true);
      T(repResult.docs_read === 1);
      T(repResult.docs_written === 1);
      T(repResult.doc_write_failures === 0);

      copy = targetDb.open(id, {conflicts: true});

      T(copy.integer === 666);
      T(copy._rev.indexOf("4-") === 0);
      T(typeof copy._conflicts === "undefined");

      var atts = copy._attachments;
      T(typeof atts === "object");
      T(typeof atts["readme.txt"] === "object");
      T(atts["readme.txt"].revpos === 3);
      T(atts["readme.txt"].content_type.indexOf("text/plain") === 0);
      T(atts["readme.txt"].stub === true);

      var att1_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
      ).responseText;
      T(att1_copy.length === att1_data.length);
      T(att1_copy === att1_data);

      T(typeof atts["data.dat"] === "object");
      T(atts["data.dat"].revpos === 4);
      T(atts["data.dat"].content_type.indexOf("application/binary") === 0);
      T(atts["data.dat"].stub === true);

      var att2_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
      ).responseText;
      T(att2_copy.length === att2_data.length);
      T(att2_copy === att2_data);


      // generate a conflict throught replication by doc IDs
      id = target_doc_ids[j].conflict_id;
      doc = sourceDb.open(id);
      copy = targetDb.open(id);
      T(doc !== null);
      T(copy !== null);
      doc.integer += 100;
      copy.integer += 1;
      T(sourceDb.save(doc).ok);
      T(targetDb.save(copy).ok);

      repResult = CouchDB.new_replicate(
        dbPairs[i].source,
        dbPairs[i].target,
        {
          body: {
            doc_ids: [id]
          }
        }
      );

      T(repResult.ok === true);
      T(repResult.docs_read === 1);
      T(repResult.docs_written === 1);
      T(repResult.doc_write_failures === 0);

      copy = targetDb.open(id, {conflicts: true});

      T(copy._rev.indexOf("5-") === 0);
      T(copy._conflicts instanceof Array);
      T(copy._conflicts.length === 1);
      T(copy._conflicts[0].indexOf("5-") === 0);
    }
  }


  docs = makeDocs(1, 25);
  docs.push({
    _id: "_design/foo",
    language: "javascript"
  });

  for (i = 0; i < dbPairs.length; i++) {
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    // add some attachments
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "readme.txt", att1_data, "text/plain");
    }

    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          continuous: true
        }
      }
    );
    T(repResult.ok === true);
    T(typeof repResult._local_id === "string");

    var rep_id = repResult._local_id;

    waitForSeq(sourceDb, targetDb);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      T(compareObjects(doc, copy) === true);

      if (j >= 10 && j < 15) {
        var atts = copy._attachments;
        T(typeof atts === "object");
        T(typeof atts["readme.txt"] === "object");
        T(atts["readme.txt"].revpos === 2);
        T(atts["readme.txt"].content_type.indexOf("text/plain") === 0);
        T(atts["readme.txt"].stub === true);

        var att_copy = CouchDB.request(
          "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
        ).responseText;
        T(att_copy.length === att1_data.length);
        T(att_copy === att1_data);
      }
    }

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    // add attachments to docs in source
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "data.dat", att2_data, "application/binary");
    }

    var ddoc = docs[docs.length - 1]; // design doc
    addAtt(sourceDb, ddoc, "readme.txt", att1_data, "text/plain");

    waitForSeq(sourceDb, targetDb);

    var modifDocs = docs.slice(10, 15).concat([ddoc]);
    for (j = 0; j < modifDocs.length; j++) {
      doc = modifDocs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      T(compareObjects(doc, copy) === true);

      var atts = copy._attachments;
      T(typeof atts === "object");
      T(typeof atts["readme.txt"] === "object");
      T(atts["readme.txt"].revpos === 2);
      T(atts["readme.txt"].content_type.indexOf("text/plain") === 0);
      T(atts["readme.txt"].stub === true);

      var att1_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
      ).responseText;
      T(att1_copy.length === att1_data.length);
      T(att1_copy === att1_data);

      if (doc._id.indexOf("_design/") === -1) {
        T(typeof atts["data.dat"] === "object");
        T(atts["data.dat"].revpos === 3);
        T(atts["data.dat"].content_type.indexOf("application/binary") === 0);
        T(atts["data.dat"].stub === true);

        var att2_copy = CouchDB.request(
          "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
        ).responseText;
        T(att2_copy.length === att2_data.length);
        T(att2_copy === att2_data);
      }
    }

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    // add another attachment to the ddoc on source
    addAtt(sourceDb, ddoc, "data.dat", att2_data, "application/binary");

    waitForSeq(sourceDb, targetDb);

    copy = targetDb.open(ddoc._id);
    var atts = copy._attachments;
    T(typeof atts === "object");
    T(typeof atts["readme.txt"] === "object");
    T(atts["readme.txt"].revpos === 2);
    T(atts["readme.txt"].content_type.indexOf("text/plain") === 0);
    T(atts["readme.txt"].stub === true);

    var att1_copy = CouchDB.request(
      "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
    ).responseText;
    T(att1_copy.length === att1_data.length);
    T(att1_copy === att1_data);

    T(typeof atts["data.dat"] === "object");
    T(atts["data.dat"].revpos === 3);
    T(atts["data.dat"].content_type.indexOf("application/binary") === 0);
    T(atts["data.dat"].stub === true);

    var att2_copy = CouchDB.request(
      "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
    ).responseText;
    T(att2_copy.length === att2_data.length);
    T(att2_copy === att2_data);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);


    // add more docs to source
    var newDocs = makeDocs(25, 35);
    populateDb(sourceDb, newDocs, true);

    waitForSeq(sourceDb, targetDb);

    for (j = 0; j < newDocs.length; j++) {
      doc = newDocs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      T(compareObjects(doc, copy) === true);
    }

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    T(sourceInfo.doc_count === targetInfo.doc_count);

    // delete docs from source
    T(sourceDb.deleteDoc(newDocs[0]).ok);
    T(sourceDb.deleteDoc(newDocs[6]).ok);

    waitForSeq(sourceDb, targetDb);

    copy = targetDb.open(newDocs[0]._id);
    T(copy === null);
    copy = targetDb.open(newDocs[6]._id);
    T(copy === null);

    var changes = targetDb.changes({since: targetInfo.update_seq});
    var line1 = changes.results[changes.results.length - 2];
    var line2 = changes.results[changes.results.length - 1];
    T(line1.id === newDocs[0]._id);
    T(line1.deleted === true);
    T(line2.id === newDocs[6]._id);
    T(line2.deleted === true);

    // cancel the replication
    repResult = CouchDB.new_replicate(
      dbPairs[i].source,
      dbPairs[i].target,
      {
        body: {
          continuous: true,
          cancel: true
        }
      }
    );
    T(repResult.ok === true);
    T(repResult._local_id === rep_id);

    doc = {
      _id: 'foobar',
      value: 666
    };
    T(sourceDb.save(doc).ok);

    wait(2000);
    copy = targetDb.open(doc._id);
    T(copy === null);
  }


  //
  // test replication triggered by non admins
  //

  // case 1) user triggering the replication is not a DB admin of the target DB
  var joeUserDoc = CouchDB.prepareUserDoc({
    name: "joe",
    roles: ["erlanger"]
  }, "erly");
  var usersDb = new CouchDB("test_suite_auth", {"X-Couch-Full-Commit":"false"});
  var server_config = [
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    }
  ];

  docs = makeDocs(1, 6);
  docs.push({
    _id: "_design/foo",
    language: "javascript"
  });

  dbPairs = [
    {
      source: sourceDb.name,
      target: targetDb.name
    },
    {
      source: "http://" + host + "/" + sourceDb.name,
      target: targetDb.name
    },
    {
      source: sourceDb.name,
      target: "http://joe:erly@" + host + "/" + targetDb.name
    },
    {
      source: "http://" + host + "/" + sourceDb.name,
      target: "http://joe:erly@" + host + "/" + targetDb.name
    }
  ];

  for (i = 0; i < dbPairs.length; i++) {
    usersDb.deleteDb();
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    T(targetDb.setSecObj({
      admins: {
        names: ["superman"],
        roles: ["god"]
      }
    }).ok);

    run_on_modified_server(server_config, function() {
      delete joeUserDoc._rev;
      T(usersDb.save(joeUserDoc).ok);

      T(CouchDB.login("joe", "erly").ok);
      T(CouchDB.session().userCtx.name === "joe");

      repResult = CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);

      T(CouchDB.logout().ok);

      T(repResult.ok === true);
      T(repResult.history[0].docs_read === docs.length);
      T(repResult.history[0].docs_written === (docs.length - 1)); // 1 ddoc
      T(repResult.history[0].doc_write_failures === 1);
    });

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if (doc._id.indexOf("_design/") === 0) {
        T(copy === null);
      } else {
        T(copy !== null);
        T(compareObjects(doc, copy) === true);
      }
    }
  }

  // case 2) user triggering the replication is not a reader (nor admin) of the
  //         source DB
  dbPairs = [
    {
      source: sourceDb.name,
      target: targetDb.name
    },
    {
      source: "http://joe:erly@" + host + "/" + sourceDb.name,
      target: targetDb.name
    },
    {
      source: sourceDb.name,
      target: "http://" + host + "/" + targetDb.name
    },
    {
      source: "http://joe:erly@" + host + "/" + sourceDb.name,
      target: "http://" + host + "/" + targetDb.name
    }
  ];

  for (i = 0; i < dbPairs.length; i++) {
    usersDb.deleteDb();
    populateDb(sourceDb, docs);
    populateDb(targetDb, []);

    T(sourceDb.setSecObj({
      admins: {
        names: ["superman"],
        roles: ["god"]
      },
      readers: {
        names: ["john"],
        roles: ["secret"]
      }
    }).ok);

    run_on_modified_server(server_config, function() {
      delete joeUserDoc._rev;
      T(usersDb.save(joeUserDoc).ok);

      T(CouchDB.login("joe", "erly").ok);
      T(CouchDB.session().userCtx.name === "joe");

      try {
        CouchDB.new_replicate(dbPairs[i].source, dbPairs[i].target);
        T(false, "should have raised an exception");
      } catch (x) {
      }

      T(CouchDB.logout().ok);
    });

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);
      T(copy === null);
    }
  }


  // cleanup
  usersDb.deleteDb();
  sourceDb.deleteDb();
  targetDb.deleteDb();
}