const supertest = require('supertest');
const chai = require('chai');
const expect = chai.expect;
const request = supertest('http://localhost:2226');  // Adjust base URL if necessary

describe('Allergy Catalog Routes', function () {

  // Test POST /create
  describe('POST /allergiesCatalog/create', function () {
    it('should create a new allergy catalog item', function (done) {
      const newItem = {
        code: 'A001',
        designation: 'Peanut',
        description: 'An allergy to peanuts.'
      };

      request.post('/api2/allergiesCatalog/create')
        .send(newItem)
        .expect(200)
        .expect('Content-Type', /json/)
        .end(function (err, res) {
          if (err) return done(err);

          // Ensure response is successful and correct format
          expect(res.body).to.have.property('code', 'A001');
          expect(res.body).to.have.property('designation', 'Peanut');
          expect(res.body).to.have.property('description', 'An allergy to peanuts.');
          done();
        });
    });

    it('should return an error if code is missing', function (done) {
      const invalidItem = {
        designation: 'Peanut'
      };

      request.post('/api2/allergiesCatalog/create')
        .send(invalidItem)
        .expect(500)  // Expect validation error
        .end(done);
    });
  });

  // Test GET /getAll
  describe('GET /allergiesCatalog/getAll', function () {
    it('should return all allergy catalog items', function (done) {
      request.get('/api2/allergiesCatalog/getAll')
        .expect(200)
        .expect('Content-Type', /json/)
        .end(function (err, res) {
          if (err) return done(err);

          // Ensure response is an array
          expect(res.body).to.be.an('array');
          done();
        });
    });
  });



  it('should return an error if code does not exist', function (done) {
    const updateData = {
      designation: 'Non-existent Allergy',
    };

    request.patch('/api2/allergiesCatalog/update/XYZ123')
      .send(updateData)
      .expect(400)  // Expect not found
      .end(done);
  });
});

// Test GET /get/:code
describe('GET /allergiesCatalog/get/:code', function () {
  it('should return a specific allergy catalog item', function (done) {
    request.get('/api2/allergiesCatalog/get/A001')
      .expect(200)
      .expect('Content-Type', /json/)
      .end(function (err, res) {
        if (err) return done(err);

        // Ensure response contains correct item
        expect(res.body).to.have.property('code', 'A001');
        expect(res.body).to.have.property('designation', 'Peanut');
        done();
      });
  });

  it('should return 404 for a non-existing allergy', function (done) {
    request.get('/allergiesCatalog/get/XYZ123')
      .expect(404)  // Expect not found
      .end(done);
  });
});

// Test PATCH /update/:code
describe('PATCH /allergiesCatalog/update/:code', function () {
  it('should update an allergy catalog item', function (done) {
    const updateData = {
      designation: 'Updated Peanut Allergy',
      description: 'Updated description of peanut allergy.'
    };

    request.patch('/api2/allergiesCatalog/update/A001')
      .send(updateData)
      .expect(200)
      .expect('Content-Type', /json/)
      .end(function (err, res) {
        if (err) return done(err);

        // Verify updated fields
        expect(res.body).to.have.property('designation', 'Updated Peanut Allergy');
        expect(res.body).to.have.property('description', 'Updated description of peanut allergy.');
        done();
      });
  });

  // Test DELETE /delete/:code
  describe('DELETE /allergiesCatalog/delete/:code', function () {
    it('should delete an allergy catalog item', function (done) {
      request.delete('/api2/allergiesCatalog/delete/A001')
        .expect(204)
        .end(function (err, res) {
          if (err) return done(err);

          // Ensure the response is success
            expect(res.body).to.be.empty;
          done();
        });
    });

    it('should return 404 if item does not exist', function (done) {
      request.delete('/api2/allergiesCatalog/delete/XYZ123')
        .expect(404)  // Expect not found
        .end(done);
    });
  });

});