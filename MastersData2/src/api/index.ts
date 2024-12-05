import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import allergyCatalog from './routes/allergyCatalogRoute';
import medicalCondition from './routes/medicalConditionRoute';
import medicalRecord from './routes/medicalRecordRoute';

export default () => {
	const app = Router();

	auth(app);
	user(app);
	role(app);
	allergyCatalog(app);
	medicalCondition(app);
	medicalRecord(app);
	
	return app
}