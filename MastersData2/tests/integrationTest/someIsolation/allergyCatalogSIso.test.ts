import { Request, Response, NextFunction } from 'express';
import AllergyCatalogController from '../../../src/controllers/allergyCatalogController';
import AllergyCatalogService from '../../../src/services/AllergyCatalogService';
import AllergyCatalogRepository from '../../../src/repos/allergyCatalogRepo'; // Assuming the repository exists
import { Result } from '../../../src/core/logic/Result';
import { Mocked } from 'ts-jest';

jest.mock('../../../src/services/AllergyCatalogService'); // Mock the service
jest.mock('../../../src/repos/allergyCatalogRepo'); // Mock the repository

describe('AllergyCatalogController Tests with Mocked Repository', () => {
  let controller: AllergyCatalogController;
  let allergyServiceMock: Mocked<AllergyCatalogService>;
  let allergyRepoMock: Mocked<AllergyCatalogRepository>; // Mock repository
  let req: Partial<Request>;
  let res: Partial<Response>;
  let next: NextFunction;

  beforeEach(() => {
    // Initialize the mock repository
    allergyRepoMock = new (AllergyCatalogRepository as any)();
    allergyServiceMock = new (AllergyCatalogService as any)();
    controller = new AllergyCatalogController(allergyServiceMock);
    
    req = {
      params: { code: '123' },
      body: { name: 'Peanut', description: 'Nut allergy' },
    };
    res = {
      json: jest.fn().mockReturnThis(),
      status: jest.fn().mockReturnThis(),
    };
    next = jest.fn();
  });

  describe('getAllergyCatalogItem', () => {
    it('should return 200 with allergy data on success', async () => {
      // Mocking repository method directly
      const mockAllergyData = { name: 'Peanut', description: 'Nut allergy' };
      allergyRepoMock.findByCode.mockResolvedValue(mockAllergyData);  // Mock the repository call
      allergyServiceMock.getAllergyCatalogItem.mockResolvedValue(Result.ok(mockAllergyData));

      await controller.getAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockAllergyData);
    });

    it('should return 404 if allergy not found', async () => {
      // Mock repository to simulate not found error
      allergyRepoMock.findByCode.mockResolvedValue(null);  // Simulate not found
      allergyServiceMock.getAllergyCatalogItem.mockResolvedValue(Result.fail('Not found'));

      await controller.getAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      // Simulate an error in the service or repository
      const error = new Error('Service error');
      allergyRepoMock.findByCode.mockRejectedValue(error);
      allergyServiceMock.getAllergyCatalogItem.mockRejectedValue(error);

      await controller.getAllergyCatalogItem(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('updateAllergyCatalogItem', () => {
    it('should return 200 with updated allergy data', async () => {
      const mockUpdatedData = { name: 'Peanut', description: 'Updated description' };
      allergyRepoMock.findByCode.mockResolvedValue(mockUpdatedData);
      allergyServiceMock.updateAllergyCatalogItem.mockResolvedValue(Result.ok(mockUpdatedData));

      await controller.updateAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockUpdatedData);
    });

    it('should return 400 if update fails', async () => {
      allergyRepoMock.findByCode.mockResolvedValue(null);  // Simulate update failure
      allergyServiceMock.updateAllergyCatalogItem.mockResolvedValue(Result.fail('Failed to update'));

      await controller.updateAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(400);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      allergyRepoMock.findByCode.mockRejectedValue(error);
      allergyServiceMock.updateAllergyCatalogItem.mockRejectedValue(error);

      await controller.updateAllergyCatalogItem(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('createAllergyCatalogItem', () => {
    it('should return 201 with created allergy data', async () => {
      const mockCreatedData = { name: 'Peanut', description: 'Nut allergy' };
      allergyRepoMock.save.mockResolvedValue(mockCreatedData);
      allergyServiceMock.createAllergyCatalogItem.mockResolvedValue(Result.ok(mockCreatedData));

      await controller.createAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith(mockCreatedData);
    });

    it('should return 400 if creation fails', async () => {
      allergyRepoMock.save.mockResolvedValue(null);  // Simulate creation failure
      allergyServiceMock.createAllergyCatalogItem.mockResolvedValue(Result.fail('Failed to create'));

      await controller.createAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(400);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      allergyRepoMock.save.mockRejectedValue(error);
      allergyServiceMock.createAllergyCatalogItem.mockRejectedValue(error);

      await controller.createAllergyCatalogItem(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('getAllAllergiesItemCatalog', () => {
    it('should return 200 with list of allergies', async () => {
      const mockAllergiesList = [
        { name: 'Peanut', description: 'Nut allergy' },
        { name: 'Pollen', description: 'Airborne allergy' },
      ];
      allergyRepoMock.findAll.mockResolvedValue(mockAllergiesList);
      allergyServiceMock.listAllergiesCatalogItems.mockResolvedValue(Result.ok(mockAllergiesList));

      await controller.getAllAllergiesItemCatalog(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockAllergiesList);
    });

    it('should return 404 if allergies list is empty', async () => {
      allergyRepoMock.findAll.mockResolvedValue([]);  // Simulate empty list
      allergyServiceMock.listAllergiesCatalogItems.mockResolvedValue(Result.fail('Not found'));

      await controller.getAllAllergiesItemCatalog(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      allergyRepoMock.findAll.mockRejectedValue(error);
      allergyServiceMock.listAllergiesCatalogItems.mockRejectedValue(error);

      await controller.getAllAllergiesItemCatalog(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });

  describe('deleteAllergyCatalogItem', () => {
    it('should return 204 on success', async () => {
      allergyRepoMock.delete.mockResolvedValue(undefined);
      allergyServiceMock.deleteAllergyCatalogItem.mockResolvedValue(Result.ok<void>(undefined));

      await controller.deleteAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(204);
    });

    it('should return 404 if allergy to delete is not found', async () => {
      allergyRepoMock.delete.mockResolvedValue(null);  // Simulate not found
      allergyServiceMock.deleteAllergyCatalogItem.mockResolvedValue(Result.fail('Not found'));

      await controller.deleteAllergyCatalogItem(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(404);
    });

    it('should call next with error if an exception occurs', async () => {
      const error = new Error('Service error');
      allergyRepoMock.delete.mockRejectedValue(error);
      allergyServiceMock.deleteAllergyCatalogItem.mockRejectedValue(error);

      await controller.deleteAllergyCatalogItem(req as Request, res as Response, next);

      expect(next).toHaveBeenCalledWith(error);
    });
  });
});