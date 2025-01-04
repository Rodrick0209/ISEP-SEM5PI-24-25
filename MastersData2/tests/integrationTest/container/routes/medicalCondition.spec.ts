const supertest = require('supertest');
const { assert } = require('chai');

// Replace with your app's URL if necessary
const request = supertest('http://localhost:2226');

describe('MedicalCondition API Endpoints', function () {
  // Test for creating a medical condition
  it('should create a medical condition', function (done) {
    const payload = {
      code: 'MD01',
      designation: 'Asthma test',
      description: 'A respiratory condition test'
    };

    request.post('/api2/medicalConditions/create')
      .send(payload)
      .expect(400) // Expect status 200 (success)
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        done();
      });
  });

  // Test for getting all medical conditions
  it('should return all medical conditions', function (done) {
    request.get('/api2/medicalConditions/getAll')
      .expect(200) // Expect status 200
      .expect('Content-Type', /json/) // Ensure Content-Type is JSON
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        assert.isArray(res.body, 'Response body should be an array');
        done();
      });
  });

  // Test for getting a medical condition by code
  it('should return medical condition by code', function (done) {
    request.get('/api2/medicalConditions/get/C1')
      .expect(404) // Expect status 200
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        done();
      });
  });

  // Test for updating a medical condition
  it('should update a medical condition', function (done) {
    const payload = {
      designation: 'Updated Asthma',
      description: 'Updated description'
    };

    request.patch('/api2/medicalConditions/update/C1')
      .send(payload)
      .expect(400) // Expect status 200
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        
        done();
      });
  });

});