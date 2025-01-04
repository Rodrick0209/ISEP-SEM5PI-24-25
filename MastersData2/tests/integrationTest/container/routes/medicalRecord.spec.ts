const supertest = require('supertest');
const { assert } = require('chai');

// Replace with your app's URL if necessary
const request = supertest('http://localhost:2226'); 

describe('Medical Record API Endpoints', function () {
  // Test for creating a medical record
  it('should create a medical record', function (done) {
    const payload = {
      patientId: '12345',
      allergies: [{ name: 'Peanuts', description: 'Severe reaction' }],
      medicalConditions: [{ name: 'Asthma', date: '2020-01-01' }]
    };

    request.post('/api2/medicalRecord/create')
      .send(payload)
      .expect(200) // Expect status 200 (success)
      .expect('Content-Type', /json/) // Ensure Content-Type is JSON
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        assert.property(res.body, 'patientId', 'Response should have patientId');
        assert.property(res.body, 'allergies', 'Response should have allergies');
        assert.property(res.body, 'medicalConditions', 'Response should have medicalConditions');
        
        done();
      });
  });

  // Test for getting all medical records
  it('should return all medical records', function (done) {
    request.get('/api2/medicalRecord/getAll')
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

  // Test for getting medical records by patientId
  it('should return medical record for a specific patient', function (done) {
    request.get('/api2/medicalRecord/getByPatientId/12345')
      .expect(200) // Expect status 200
      .expect('Content-Type', /json/) // Ensure Content-Type is JSON
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        assert.property(res.body, 'patientId', 'Response should have patientId');
        assert.strictEqual(res.body.patientId, '12345', 'Patient ID should match');
        
        done();
      });
  });

  // Test for searching medical records by patientId and name
  it('should search medical records for a patient by name', function (done) {
    request.get('/api2/medicalRecord/search/12345/Asthma')
      .expect(200) // Expect status 200
      .expect('Content-Type', /json/) // Ensure Content-Type is JSON
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        assert.isNotNull(res.body, 'Response body should not be null');
        done();
      });
  });

  // Test for updating a medical record
  it('should update a medical record', function (done) {
    const payload = {
      allergies: [{ code: 'A1', designation: 'Peanuts', description: 'Updated description' }],
      medicalConditions: [{ code: 'M1', designation: 'Asthma', date: '2022-02-02' }]
    };

    request.put('/api2/medicalRecord/update/12345')
      .send(payload)
      .expect(200) // Expect status 200
      .expect('Content-Type', /json/) // Ensure Content-Type is JSON
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        assert.property(res.body, 'patientId', 'Response should have patientId');
        done();
      });
  });

  // Test for dropping all medical records
  it('should drop all medical records', function (done) {
    request.delete('/api2/medicalRecord/dropAll')
      .expect(200) // Expect status 200
      .end(function (err, res) {
        if (err) {
          console.error("Error:", err);
          return done(err);
        }

        // For debugging
        console.log('Response body:', res.body);
        
        assert.strictEqual(res.text, 'All medical records have been deleted.', 'Response should confirm deletion');
        done();
      });
  });
});