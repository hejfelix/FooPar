package it.vigtig.foopar.test;

public class MatrixOps {

	public static void mult(double[] A, double[] B, double[] C, int M, int N,
			int K) {
		for (int i = 0; i < M; i++) {
			for (int j = 0; j < N; j++) {
				for (int k = 0; k < K; k++) {
					C[i + j * M] += A[i + k * M] * B[k + j * M];
				}
			}
		}
	}

}
