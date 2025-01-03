import { Request, Response, NextFunction } from 'express';
import MedicalRecordController from '../../../src/controllers/MedicalRecordController';
import MedicalRecordService from '../../../src/services/MedicalRecordService';
import MedicalRecordRepository from '../../../src/repos/MedicalRecordRepo';
import { Result } from '../../../src/core/logic/Result';
import { Mocked } from 'ts-jest';

jest.mock('../../../src/services/MedicalRecordService');
jest.mock('../../../src/repos/MedicalRecordRepo');

describe('MedicalRecordController Tests with Mocked Repository', () => {
  let controller: MedicalRecordController;
  let medicalRecordServiceMock: Mocked<MedicalRecordService>;
  let medicalRecordRepoMock: Mocked<MedicalRecordRepository>;
  let req: Partial<Request>;
  let res: Partial<Response>;
  let next: NextFunction;

  beforeEach(() => {
    medicalRecordRepoMock = new (MedicalRecordRepository as any)();
    medicalRecordServiceMock = new (MedicalRecordService as any)();
    controller = new MedicalRecordController(medicalRecordServiceMock);

    req = {
      params: { patientId: '123', id: '456' },
      body: { name: 'Diabetes', description: 'Chronic illness' },
    };
    res = {
      json: jest.fn().mockReturnThis(),
      status: jest.fn().mockReturnThis(),
    };
    next = jest.fn();
  });

  describe('createMedicalRecord', () => {
    it('should return 201 with created medical record', async () => {
      const mockCreatedRecord = { name: 'Diabetes', description: 'Chronic illness' };
      medicalRecordServiceMock.createMedicalRecord.mockResolvedValue(Result.ok(mockCreatedRecord));

      await controller.createMedicalRecord(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith(mockCreatedRecord);
    });

    it('should return 402 if creation fails', async () => {
      medicalRecordServiceMock.createMedicalRecord.mockResolvedValue(Result.fail('Failed to create'));

      await controller.createMedicalRecord(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(402);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalRecordServiceMock.createMedicalRecord.mockRejectedValue(error);

      await controller.createMedicalRecord(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('getAllMedicalRecord', () => {
    it('should return 200 with list of medical records', async () => {
      const mockRecords = [{ name: 'Diabetes', description: 'Chronic illness' }];
      medicalRecordServiceMock.listMedicalRecords.mockResolvedValue(Result.ok(mockRecords));

      await controller.getAllMedicalRecord(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockRecords);
    });

    it('should return 404 if no records are found', async () => {
      medicalRecordServiceMock.listMedicalRecords.mockResolvedValue(Result.fail('Not found'));

      await controller.getAllMedicalRecord(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalRecordServiceMock.listMedicalRecords.mockRejectedValue(error);

      await controller.getAllMedicalRecord(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('updateMedicalRecord', () => {
    it('should return 200 with updated record', async () => {
      const mockUpdatedRecord = { name: 'Diabetes', description: 'Updated description' };
      medicalRecordServiceMock.updateMedicalRecord.mockResolvedValue(Result.ok(mockUpdatedRecord));

      await controller.updateMedicalRecord(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockUpdatedRecord);
    });

    it('should return 400 if update fails', async () => {
      medicalRecordServiceMock.updateMedicalRecord.mockResolvedValue(Result.fail('Failed to update'));

      await controller.updateMedicalRecord(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(400);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalRecordServiceMock.updateMedicalRecord.mockRejectedValue(error);

      await controller.updateMedicalRecord(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('getMedicalRecordByPatientId', () => {
    it('should return 200 with medical record data', async () => {
      const mockRecord = { name: 'Diabetes', description: 'Chronic illness' };
      medicalRecordServiceMock.getMedicalRecordByPatientId.mockResolvedValue(Result.ok(mockRecord));

      await controller.getMedicalRecordByPatientId(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockRecord);
    });

    it('should return 404 if record is not found', async () => {
      medicalRecordServiceMock.getMedicalRecordByPatientId.mockResolvedValue(Result.fail('Not found'));

      await controller.getMedicalRecordByPatientId(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalRecordServiceMock.getMedicalRecordByPatientId.mockRejectedValue(error);

      await controller.getMedicalRecordByPatientId(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });
});