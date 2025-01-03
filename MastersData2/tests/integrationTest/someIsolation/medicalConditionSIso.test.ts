import { Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import MedicalConditionController from '../../../src/controllers/medicalConditionController';
import MedicalConditionService from '../../../src/services/MedicalConditionService';
import MedicalConditionRepo from '../../../src/repos/medicalConditionRepo';
import { Mocked } from 'ts-jest';
import { Result } from '../../../src/core/logic/Result';

jest.mock('../../../src/services/MedicalConditionService'); // Mock the service
jest.mock('../../../src/repos/medicalConditionRepo'); // Mock the repository

describe('MedicalConditionController Tests', () => {
  let controller: MedicalConditionController;
  let medicalConditionServiceMock: Mocked<MedicalConditionService>;
  let medicalConditionRepoMock: Mocked<MedicalConditionRepo>; // Mock repository
  let req: Partial<Request>;
  let res: Partial<Response>;
  let next: NextFunction;

  beforeEach(() => {
    medicalConditionRepoMock = new (MedicalConditionRepo as any)();
    medicalConditionServiceMock = new (MedicalConditionService as any)();
    Container.set('MedicalConditionService', medicalConditionServiceMock);
    controller = new MedicalConditionController(medicalConditionServiceMock);

    req = {
      params: { id: '123' },
      body: { name: 'Diabetes', description: 'Chronic condition' },
    };
    res = {
      json: jest.fn().mockReturnThis(),
      status: jest.fn().mockReturnThis(),
    };
    next = jest.fn();
  });

  describe('getMedicalCondition', () => {
    it('should return 200 with medical condition data on success', async () => {
      const mockConditionData = { name: 'Diabetes', description: 'Chronic condition' };
      medicalConditionServiceMock.getMedicalCondition.mockResolvedValue(Result.ok(mockConditionData));

      await controller.getMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockConditionData);
    });

    it('should return 404 if medical condition not found', async () => {
      medicalConditionServiceMock.getMedicalCondition.mockResolvedValue(Result.fail('Not found'));

      await controller.getMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalConditionServiceMock.getMedicalCondition.mockRejectedValue(error);

      await controller.getMedicalCondition(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('updateMedicalCondition', () => {
    it('should return 200 with updated medical condition data', async () => {
      const mockUpdatedData = { name: 'Diabetes', description: 'Updated description' };
      medicalConditionServiceMock.updateMedicalCondition.mockResolvedValue(Result.ok(mockUpdatedData));

      await controller.updateMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockUpdatedData);
    });

    it('should return 400 if update fails', async () => {
      medicalConditionServiceMock.updateMedicalCondition.mockResolvedValue(Result.fail('Failed to update'));

      await controller.updateMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(400);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalConditionServiceMock.updateMedicalCondition.mockRejectedValue(error);

      await controller.updateMedicalCondition(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('createMedicalCondition', () => {
    it('should return 201 with created medical condition data', async () => {
      const mockCreatedData = { name: 'Diabetes', description: 'Chronic condition' };
      medicalConditionServiceMock.createMedicalCondition.mockResolvedValue(Result.ok(mockCreatedData));

      await controller.createMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith(mockCreatedData);
    });

    it('should return 400 if creation fails', async () => {
      medicalConditionServiceMock.createMedicalCondition.mockResolvedValue(Result.fail('Failed to create'));

      await controller.createMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(400);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalConditionServiceMock.createMedicalCondition.mockRejectedValue(error);

      await controller.createMedicalCondition(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('deleteMedicalCondition', () => {
    it('should return 204 on success', async () => {
      medicalConditionServiceMock.deleteMedicalCondition.mockResolvedValue(Result.ok<void>(undefined));

      await controller.deleteMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(204);
    });

    it('should return 404 if medical condition to delete is not found', async () => {
      medicalConditionServiceMock.deleteMedicalCondition.mockResolvedValue(Result.fail('Not found'));

      await controller.deleteMedicalCondition(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      medicalConditionServiceMock.deleteMedicalCondition.mockRejectedValue(error);

      await controller.deleteMedicalCondition(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });
});