import { expect } from 'chai';
import sinon from 'sinon';
import { Request, Response, NextFunction } from 'express';
import MedicalRecordController from '../../src/controllers/medicalRecordController';
import IMedicalRecordService from '../../src/services/IServices/IMedicalRecordService';
import { Result } from '../../src/core/logic/Result';
import IMedicalRecordDTO from '../../src/dto/IMedicalRecordDTO';

describe('MedicalRecordController', () => {
  let medicalRecordServiceInstance: IMedicalRecordService;
  let medicalRecordController: MedicalRecordController;
  let req: Partial<Request>;
  let res: Partial<Response>;
  let next: NextFunction;

  beforeEach(() => {
    medicalRecordServiceInstance = {
      createMedicalRecord: sinon.stub(),
      listMedicalRecords: sinon.stub(),
      updateMedicalRecord: sinon.stub(),
      searchMedicalRecordEntries: sinon.stub(),
    } as any;
    medicalRecordController = new MedicalRecordController(medicalRecordServiceInstance);
    req = {
      params: {},
      query: {},
      body: {},
    };
    res = {
      status: sinon.stub().returnsThis(),
      json: sinon.stub().returnsThis(),
      send: sinon.stub().returnsThis(),
    };
    next = (err?: any) => {};
  });

  it('should display search results for allergies', async () => {
    const medicalRecordDTO: IMedicalRecordDTO[] = [{ patientId: '123',  allergies: ['Peanut'], medicalConditions: [] }];
    (medicalRecordServiceInstance.searchMedicalRecordEntries as sinon.SinonStub).resolves(Result.ok<IMedicalRecordDTO[]>(medicalRecordDTO));

    req.params = req.params || {};
    req.params.patientId = '123';
    req.query = req.query || {};
    req.query.name = 'Peanut';

    await medicalRecordController.searchMedicalRecordEntries(req as Request, res as Response, next);

    expect(res.status).to.have.been.calledWith(200);
    expect(res.json).to.have.been.calledWith(medicalRecordDTO);
  });

  it('should display search results for medical conditions', async () => {
    const medicalRecordDTO: IMedicalRecordDTO[] = [{ patientId: '123',  allergies: [], medicalConditions: ['Diabetes'] }];
    (medicalRecordServiceInstance.searchMedicalRecordEntries as sinon.SinonStub).resolves(Result.ok<IMedicalRecordDTO[]>(medicalRecordDTO));

    req.params = req.params || {};
    req.params.patientId = '123';
    req.query = req.query || {};
    req.query.name = 'Diabetes';

    await medicalRecordController.searchMedicalRecordEntries(req as Request, res as Response, next);

    expect(res.status).to.have.been.calledWith(200);
    expect(res.json).to.have.been.calledWith(medicalRecordDTO);
  });
});