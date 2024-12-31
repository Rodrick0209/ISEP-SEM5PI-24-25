import { Request, Response, NextFunction } from 'express';
import MedicalRecordController from '../../../src/controllers/medicalRecordController';
import IMedicalRecordService from '../../../src/services/IServices/IMedicalRecordService';
import { Result } from '../../../src/core/logic/Result';
import IMedicalRecordDTO from '../../../src/dto/IMedicalRecordDTO';
/*
describe('MedicalRecordController', () => {
  let medicalRecordServiceInstance: jest.Mocked<IMedicalRecordService>;
  let medicalRecordController: MedicalRecordController;
  let req: Partial<Request>;
  let res: Partial<Response>;
  let next: jest.Mocked<NextFunction>;

  beforeEach(() => {
    medicalRecordServiceInstance = {
      createMedicalRecord: jest.fn(),
      listMedicalRecords: jest.fn(),
      updateMedicalRecord: jest.fn(),
      searchMedicalRecordEntries: jest.fn(),
      getMedicalRecordByPatientId: jest.fn(),
    };

    medicalRecordController = new MedicalRecordController(medicalRecordServiceInstance);
    req = {};
    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
      send: jest.fn().mockReturnThis(),
    };
    next = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should create a medical record successfully', async () => {
    const mockDTO: IMedicalRecordDTO = { patientId: '123', allergies: [], medicalConditions: [] };
    medicalRecordServiceInstance.createMedicalRecord.mockResolvedValue(Result.ok<IMedicalRecordDTO>(mockDTO));

    req.body = mockDTO;

    await medicalRecordController.createMedicalRecord(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(201);
    expect(res.json).toHaveBeenCalledWith(mockDTO);
  });

  it('should handle failure when creating a medical record', async () => {
    medicalRecordServiceInstance.createMedicalRecord.mockResolvedValue(Result.fail('Error creating medical record'));

    req.body = { patientId: '123' };

    await medicalRecordController.createMedicalRecord(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(402);
    //expect(res.send).toHaveBeenCalledWith('Error creating medical record');
  });

  it('should list all medical records successfully', async () => {
    const mockDTOs: IMedicalRecordDTO[] = [
      { patientId: '123', allergies: [], medicalConditions: [] },
      { patientId: '456', allergies: ['Peanut'], medicalConditions: ['Diabetes'] },
    ];
    medicalRecordServiceInstance.listMedicalRecords.mockResolvedValue(Result.ok<IMedicalRecordDTO[]>(mockDTOs));

    await medicalRecordController.getAllMedicalRecord(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith(mockDTOs);
  });

  it('should handle failure when listing medical records', async () => {
    medicalRecordServiceInstance.listMedicalRecords.mockResolvedValue(Result.fail('No records found'));

    await medicalRecordController.getAllMedicalRecord(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith('No records found');
  });

  it('should update a medical record successfully', async () => {
    const mockDTO: IMedicalRecordDTO = { patientId: '123', allergies: ['Peanut'], medicalConditions: [] };
    medicalRecordServiceInstance.updateMedicalRecord.mockResolvedValue(Result.ok<IMedicalRecordDTO>(mockDTO));

    req.body = mockDTO;
    req.params = { id: '123' };

    await medicalRecordController.updateMedicalRecord(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith(mockDTO);
  });

  it('should handle failure when updating a medical record', async () => {
    medicalRecordServiceInstance.updateMedicalRecord.mockResolvedValue(Result.fail('Update failed'));

    req.body = { patientId: '123', allergies: [], medicalConditions: [] };
    req.params = { id: '123' };

    await medicalRecordController.updateMedicalRecord(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.send).toHaveBeenCalledWith('Update failed');
  });

  it('should get a medical record by patientId successfully', async () => {
    const mockDTO: IMedicalRecordDTO = { patientId: '123', allergies: [], medicalConditions: [] };
    medicalRecordServiceInstance.getMedicalRecordByPatientId.mockResolvedValue(Result.ok<IMedicalRecordDTO>(mockDTO));

    req.params = { patientId: '123' };

    await medicalRecordController.getMedicalRecordByPatientId(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith(mockDTO);
  });

  it('should handle failure when getting a medical record by patientId', async () => {
    medicalRecordServiceInstance.getMedicalRecordByPatientId.mockResolvedValue(Result.fail('Not found'));

    req.params = { patientId: '123' };

    await medicalRecordController.getMedicalRecordByPatientId(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.send).toHaveBeenCalledWith('Not found');
  });

  it('should search medical records for allergies', async () => {
    const mockDTO: IMedicalRecordDTO[] = [{ patientId: '123', allergies: ['Peanut'], medicalConditions: [] }];
    medicalRecordServiceInstance.searchMedicalRecordEntries.mockResolvedValue(Result.ok<IMedicalRecordDTO[]>(mockDTO));

    req.params = { patientId: '123' };
    req.query = { name: 'Peanut' };

    await medicalRecordController.searchMedicalRecordEntries(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith(mockDTO);
  });

  it('should search medical records for medical conditions', async () => {
    const mockDTO: IMedicalRecordDTO[] = [{ patientId: '123', allergies: [], medicalConditions: ['Diabetes'] }];
    medicalRecordServiceInstance.searchMedicalRecordEntries.mockResolvedValue(Result.ok<IMedicalRecordDTO[]>(mockDTO));

    req.params = { patientId: '123' };
    req.query = { name: 'Diabetes' };

    await medicalRecordController.searchMedicalRecordEntries(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith(mockDTO);
  });

  it('should handle failure when searching medical records', async () => {
    medicalRecordServiceInstance.searchMedicalRecordEntries.mockResolvedValue(Result.fail('No matches found'));

    req.params = { patientId: '123' };
    req.query = { name: 'Unknown' };

    await medicalRecordController.searchMedicalRecordEntries(req as Request, res as Response, next);

    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.send).toHaveBeenCalledWith('No matches found');
  });
});*/