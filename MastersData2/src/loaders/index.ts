import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';
import path from 'path';
import allergyCatalogSchema from '../persistence/schemas/allergyCatalogSchema';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');

  const userSchema = {
    // compare with the approach followed in repos and services
    name: 'userSchema',
    schema: '../persistence/schemas/userSchema',
  };

  const allergyCatalogSchema = {
    name: 'allergyCatalogSchema',
    schema: '../persistence/schemas/allergyCatalogSchema',
  };



  const roleSchema = {
    // compare with the approach followed in repos and services
    name: 'roleSchema',
    schema: '../persistence/schemas/roleSchema',
  };

  const roleController = {
    name: config.controllers.role.name,
    path: config.controllers.role.path
  }

  const roleRepo = {
    name: config.repos.role.name,
    path: config.repos.role.path
  }

  const userRepo = {
    name: config.repos.user.name,
    path: config.repos.user.path
  }

  const roleService = {
    name: config.services.role.name,
    path: config.services.role.path
  }

  const allergyCatalogController = {
    name: config.controllers.allergyCatalog.name,
    path: config.controllers.allergyCatalog.path
  }

  const allergyCatalog = {
    name: config.repos.allergyCatalog.name,
    path: config.repos.allergyCatalog.path
  }

  const allergyCatalogService = {
    name: config.services.allergyCatalog.name,
    path: config.services.allergyCatalog.path
  }



  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      userSchema,
      roleSchema,
      allergyCatalogSchema
    ],
    controllers: [
      roleController,
      allergyCatalogController
    ],
    repos: [
      roleRepo,
      userRepo,
      allergyCatalog
    ],
    services: [
      roleService,
      allergyCatalogService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
